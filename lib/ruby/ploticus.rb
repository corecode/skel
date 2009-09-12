require 'tempfile'

class Ploticus
  PLOTICUS_BIN = "pl"

  instance_methods.each { |m| undef_method m unless m =~ /^__/ }

  def initialize
    reset!
  end

  def data=(rows)
    if Array === rows and not Array === rows[0]
      rows = rows.map{|v| [v]}
    end
    data {|a| a.data = rows }
  end

  def plot!(img_type="png", pl_executable=PLOTICUS_BIN, *args)
    io = IO.popen("#{pl_executable} -stdin -o stdout -#{ img_type } #{ args.join(' ') }", "w+")
    STDERR.puts @s if $DEBUG
    io.write @s
    io.close_write
    img = io.read
    io.close
    img
  end

  def reset!
    @s = ""
  end

  PROCS = [ 
    :xaxis, :yaxis, :lineplot, :page, :legend,
    [:data, :getdata],
    [:area, :areadef], 
  ]

  PROCS.each do |m, mp|
    mp ||= m
    eval %[ def #{ m }(&block) do_block("#{ mp }", &block) end ]
  end

  def method_missing(id, &block)
    do_block(id.to_s, &block)
  end

  private

  def do_block(mp, &block)
    @s << "#proc #{ mp }\n"
    AttrWriter.new(&block).__attrs__.each do |k, v|
      @s << "#{ k }: #{ Ploticus::convert(v) }\n"
    end
    @s << "\n"
  end

  def self.convert(obj)
    case obj
    when String
      obj.gsub("\n", "\\n") + "\n"
    when Array
      if obj[0].kind_of? Array
        obj.map {|row| row.join(" ") + "\n"}.join
      else
        obj.join(" ")
      end
    when Hash
      obj.map {|k,v| "#{ k }=#{ convert(v) }" }.join(" ")
    when true
      'yes'
    when false
      'no'
    else
      obj.to_s
    end
  end

  class AttrWriter
    instance_methods.each { |m| undef_method m unless m =~ /^__/ }

    def __attrs__
      @attrs
    end

    def initialize(&block)
      @attrs = {}
      block.call(self)
    end

    def method_missing(id, value)
      @attrs[id.to_s.gsub("=", "")] = value
    end
  end
end

class PloticusPrefab
  attr_accessor :data

  def initialize(prefab)
    @prefab = prefab
    @attrs = {}
  end

  def data=(rows)
    if Array === rows and not Array === rows[0]
      rows = rows.map{|v| [v]}
    end
    @data = rows
  end

  def method_missing(id, value)
    @attrs[id.to_s.gsub("=", "")] = value
  end

  def []=(k, v)
    @attrs[k] = v
  end

  def plot!(img_type="png", pl_executable=Ploticus::PLOTICUS_BIN, *args)
    img = nil
    Tempfile.open('rbploticus') do |dataf|
      dataf.write Ploticus.convert(@data)
      dataf.close

      attrs = @attrs.map{|k,v| "#{k}=#{v}"}.join(' ')
      cmd = "#{pl_executable} -prefab #{@prefab} -o stdout -#{ img_type } data=#{dataf.path} #{attrs} #{ args.join(' ') }"
      $stderr.puts cmd if $DEBUG
      io = IO.popen(cmd, "w+")
      io.close_write
      img = io.read
      io.close
    end
    img
  end
end

if __FILE__ == $0
  pl = Ploticus.new

  pl.data = [
      [2000, 750],
      [2010, 1700],
      [2015, 2000],
      [2020, 1800],
      [2025, 1300],
      [2030, 400]
    ]

  pl.area {|a|
    a.title = "Social Security trust fund asset estimates, in $ billions\nhallo\nsuper"
    a.titledetails = {:adjust => "0,0.1"}
    a.rectangle = [1, 1, 5, 2]
    a.xrange = [2000, 2035]
    a.yrange = [0, 2000]
  }

  pl.xaxis {|a| 
    a.stubs = "inc 5"
    a.label = "Year"
  }

  pl.lineplot {|a|
    a.xfield = 1
    a.yfield = 2
    a.fill = "pink"
  }

  pl.lineplot {|a|
    a.xfield = 1
    a.yfield = 2
    a.fill = "rgb(.7,.3,.3)"
    a.linerange = [2010, 2020]
  }

  pl.lineplot {|a|
    a.xfield = 1
    a.yfield = 2
    a.linedetails = {:color => 'red'}
    a.fill = "rgb(.7,.3,.3)"
    a.linerange = [2010, 2020]
  }

  pl.yaxis {|a|
    a.stubs = "incremental 500"
    a.grid = {:color=> 'blue'}
    a.axisline = 'none'
  }

  puts pl.plot!
end
