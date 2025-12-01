#!/usr/bin/env bash

if [[ $# -ne 1 ]]; then
    echo "Day number must be provided as a command-line argument"
    exit
fi

year=2025
day_number=$1

printf -v day "%02d" $day_number
script_path="day-${day}.R"
data_path="data/full-${day}.txt"
example_path="data/example-${day}.txt"

echo "Fetching puzzle data for year ${year}, day ${day}..."
echo "---------------------------------------------"

mkdir -p data/
curl -s https://adventofcode.com/${year}/day/${day_number}/input --cookie "session=$(cat SESSION)" -o ${data_path}

if grep -Eq '404 Not Found|endpoint' $data_path; then
    echo "Data not found online!"
    rm $data_path
    exit 1
else
    sed "s/DAY <- NA/DAY <- ${day_number}/" template.R > $script_path
    echo "Solution script saved as ${script_path}"
    echo "Full data saved as ${data_path}"
    touch $example_path
    echo "Put example data in ${example_path}"
    vi ${example_path}
fi
