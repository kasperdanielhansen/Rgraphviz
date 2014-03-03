#!/bin/bash -e

## Find all #include <XX>
grep -R "#include <" * > all_includes.txt

## Clean, and sort the include statements, so only the header remains
cut -f2 -d: all_includes.txt | sort | uniq | sed 's:.*#include <::' | sed 's:\.h>.*:.h:' | grep -v ">$" | grep -v "all_includes.txt" | sort -b > all_include_statements.txt

## Find all header files
find . -iname "*.h" | sed 's:.*/::' | sort | uniq | sort -b > all_header_files.txt

## Find intersection between header files and #include <XX>; these needs to be fixed
join all_include_statements.txt all_header_files.txt > need_fixes.txt

cat need_fixes.txt | while read include
do
  FIXFILES=`grep -R -l "#include <$include>" * | grep -v all_include`
  sed -i "_tmp" "s:#include <$include>:#include \"$include\":" $FIXFILES
done

    
