#!./miniruby -s

SYM = {}

objs = ARGV.collect {|s| s.tr('/', '\\')}
IO.foreach("|dumpbin -symbols " + objs.join(' ')) do |l|
  next if /^[0-9A-F]+ 0+ UNDEF / =~ l
  next unless l.sub!(/.*\sExternal\s+\|\s+/, '')
  if l.sub!(/^_/, '')
    next if /@.*@/ =~ l
  elsif !l.sub!(/^(\S+) \([^@?]*\)$/, '\1')
    next
  end
  SYM[l.strip] = true
end

exports = []
if $name
  exports << "Name " + $name
elsif $library
  exports << "Library " + $library
end
exports << "Description " + $description.dump if $description
exports << "EXPORTS" << SYM.keys.sort

if $output
  open($output, 'w') {|f| f.puts exports}
else
  puts exports
end
