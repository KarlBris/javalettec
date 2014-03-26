#!/usr/bin/env ruby
require 'open3'

if ARGV.length < 2 then
  puts "Usage: <javalette compiler> <folder with good and bad test folders>"
  exit
end

jlc = ARGV[0]
folder = ARGV[1]

good_tests = Dir["#{folder}/good/*.jl"]
bad_tests  = Dir["#{folder}/bad/*.jl"]

num_tests  = good_tests.length + bad_tests.length
num_passed = 0 

good_tests.each do |testfile|
  _, _, stderr = Open3.popen3("#{jlc} #{testfile}")
  output = stderr.readline
  unless output =~ /^OK/ then
    puts "Test #{testfile} failed (should compile)"
  else
    num_passed += 1
  end
end

bad_tests.each do |testfile|
  _, _, stderr = Open3.popen3("#{jlc} #{testfile}")
  output = stderr.readline
  unless output =~ /^ERR/ then
    puts "Test #{testfile} compiled (should fail)"    
  else
    num_passed += 1
  end
end


puts "Passed #{num_passed} of #{num_tests} tests."

