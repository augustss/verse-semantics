#!/bin/bash

echo "=== TESTING ALL VERSE FILES ==="
echo "Parsing all 459 Verse files to find failures..."

successful=0
failed=0
declare -a failing_files=()

# Test every file
for file in $(find verse-files-flat -name "*.verse" | sort); do
    filename=$(basename "$file")

    # Test file quietly
    if npm run dev "$file" > /dev/null 2>&1; then
        ((successful++))
    else
        ((failed++))
        failing_files+=("$filename")
        echo "FAILED: $filename"
    fi

    total=$((successful + failed))

    # Progress every 50 files
    if [ $((total % 50)) -eq 0 ]; then
        echo "Progress: $total/459 files (Failed: $failed)"
    fi
done

echo
echo "=== RESULTS ==="
echo "Total: $((successful + failed))"
echo "Successful: $successful"
echo "Failed: $failed"

if [ $failed -eq 0 ]; then
    echo "🎉 ALL FILES PARSE SUCCESSFULLY!"
else
    success_rate=$((successful * 100 / (successful + failed)))
    echo "Success rate: $success_rate%"
    echo
    echo "Failing files:"
    for file in "${failing_files[@]}"; do
        echo "  ✗ $file"
    done
fi