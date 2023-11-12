#!/bin/bash
set -eo pipefail

##########
### Summary: Splits a csv file and retains the first row per new file
### Author: Anthogonyst
##########

### Pre-initializers
nrow=10000
force=0
verbose=0

### Initializers using getopts
while getopts 'fhn:v' opt;
do
  case $opt in
    f) force=1;;
    ### Prints the help instructions
    h) echo "Usage: SplitCsv.sh [-fv] [-n number of rows] path/to/file.csv"
      echo "f - Force file writing even 'split' to 1 file"
      echo "n - The number of rows desired in the output file"
      echo "v - Verbose print to console"
      echo ""
      exit 0;;
    ### Specify the number of rows
    n) nrow="$OPTARG";;
    v) verbose=1;;
    ### Error values
    \?) echo "Invalid option: -$OPTARG" >&2; exit 1;;
    :) echo "Missing option argument for -$OPTARG" >&2; exit 1;;
    *) echo "Unimplemented option: -$OPTARG" >&2; exit 1;;
esac
done
shift "$(($OPTIND -1))"

### Post-initializers
filein=$(readlink -f "$1")
filename="${filein%.*}"
fileext="${filein##*.}"
#fileout="$filename".Part"$i"."$fileext"


### Error checking
if [[ -z "$filein" || ! -f "$filein" ]]; then
  echo "No file specified or does not exist; write path/to/file.csv"
  echo "$filein"
  exit 2
fi

if [ ${nrow} -lt 2 ]; then
  echo "Row number specified is negative or near zero."
  echo ${nrow}
  exit 3
fi


### Logic
fileRows=$(cat "$filein" | wc -l)
iter=$(( ($fileRows - 2) / $nrow ))
tailRows=$(( ${fileRows} - ${nrow} * ${iter} - 1 ))
tailIter=$(( ${iter} + 1 ))

### Temporary file header
head -n 1 "$filein" > "$filename"."$fileext".Header.tmp


### First N files
for (( i=1; i<=${iter}; i++ ))
do
  let "n = ${nrow} * ${i} + 1"
  head -n ${n} "$filein" | tail -n ${nrow} > "$filename".Part"$i"."$fileext".tmp
  cat "$filename"."$fileext".Header.tmp "$filename".Part"$i"."$fileext".tmp > "$filename".Part"$i"."$fileext"
  rm "$filename".Part"$i"."$fileext".tmp
done

### Last file
if [ ${force} -gt 0 ] || [[ ${tailRows} -gt 0 && ${iter} -gt 0 ]]; then
  tail -n ${tailRows} ${filein} > "$filename".Part"$tailIter"."$fileext".tmp
  cat "$filename"."$fileext".Header.tmp "$filename".Part"$tailIter"."$fileext".tmp > "$filename".Part"$tailIter"."$fileext"
  rm "$filename".Part"$tailIter"."$fileext".tmp
fi


### Delete temporary file header
rm "$filename"."$fileext".Header.tmp


### Print results
if [ ${verbose} -gt 0 ]; then
  if [ ${force} -eq 0 ] && [ ${iter} -lt 1 ]; then
    echo "The table has more rows than the split requested: $(cat "$filein" | wc -l)"
  elif [ ${tailRows} -gt 0 ]; then
    echo "Done creating ${tailIter} new tables!  :)"
  else
    echo "Done creating ${iter} new tables!"
  fi
fi
