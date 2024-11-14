#!/usr/bin/env bash

usage() {
    echo -e "\nUsage: rl -d/--dice XdY or -c/--custom X.Y,Z,.. [-m/--median] [-k/--keep <h/l>V] [-s/--sum] [-a/--add W]"
    exit 1
}

trap usage ERR SIGINT SIGABRT

roll_dice() {
    local -n result="$1"
    local -n expect="$4"
    result=()
    expect=0
    for ((i = 1; i <= "$3"; i++)); do
        expect="$((expect+i))"
    done
    expect="$(bc -l <<< "scale=1; $expect/$3")"
    for ((i = 0; i < "$2"; i++)); do
        throw="$(shuf -i 1-"$3" -n 1)"
        result+=( "$throw" )
    done
}

custom_die() {
    local -n result="$1"
    local -n expect="$4"
    result=()
    expect=0
    read -ra sides <<< "$(tr "," " " <<< "$3")"
    amount_sides="${#sides[@]}"
    for side in "${sides[@]}"; do
        expect="$((expect+side))"
    done
    expect="$(bc -l <<< "scale=1; $expect/$amount_sides")"
    for ((i = 0; i < "$2"; i++)); do
        throw="$(shuf -e "${sides[@]}" -n 1)"
        result+=( "$throw" )
    done
}

sum() {
    declare -a rolls=("${!1}")
    local -n total="$2"
    total=0
    for die in "${rolls[@]}"; do
        total="$((total+die))"
    done
}

add() {
    bon="$1"
    local -n res="$2"
    echo "$(bc -l <<< "scale=1; $res+$bon")"
}

median() {
    local -n am="$1"
    local -n expect="$2"
    echo "$(bc -l <<< "scale=1; $am*$expect")"
}

keep() {
    if [[ "${2:0:1}" == "h" ]]; then
        keep_high "$1" "$2"
    elif [[ "${2:0:1}" == "l" ]]; then
        keep_low "$1" "$2"
    else
        echo "invalid operation '${2:0:1}'"
    fi
}

keep_high() {
    local -n rolls="$1"
    read -ra highest <<< "$(tr " " "\n" <<<  "${rolls[@]}" | sort -nr | head -n"$(cut -dh -f2 <<< "$2")" | tr "\n" " ")"
    read -ra rolls <<< "${highest[@]}"
    echo "${rolls[@]}"
}

keep_low() {
    local -n rolls="$1"
    read -ra lowest <<< "$(tr " " "\n" <<<  "${rolls[@]}" | sort -n | head -n"$(cut -dl -f2 <<< "$2")" | tr "\n" " ")"
    read -ra rolls <<< "${lowest[@]}"
    echo "${rolls[@]}"
}

dice_roll=()
expectation=0
amount=0
final_sum=0

VALID_ARGS=$(getopt -o d:c:k:a:sm --long dice:,custom:,keep:,add:,sum,median -- "$@")
if [[ $? -ne 0 ]]; then
    exit 1;
fi

printf "Time of roll: "
date +%X
printf "\n"

eval set -- "$VALID_ARGS"
while [ : ]; do
    case "$1" in
        -d | --dice)
            echo "Rolling '$2'..."
            amount="$(cut -dd -f1 <<< "$2")"
            sides="$(cut -dd -f2 <<< "$2")"
            roll_dice dice_roll "$amount" "$sides" expectation
            echo "${dice_roll[@]}"
            shift 2
            ;;
        -c | --custom)
            echo "Rolling '$2'..."
            amount="$(cut -d. -f1 <<< "$2")"
            sides="$(cut -d. -f2 <<< "$2")"
            custom_die dice_roll "$amount" "$sides" expectation
            echo "${dice_roll[@]}"
            shift 2
            ;;
        -k | --keep)
            echo "Keeping '$2'..."
            keep dice_roll "$2"
            shift 2
            ;;
        -a | --add)
            echo "Adding '$2'..."
            sum dice_roll[@] final_sum
            add "$2" final_sum
            shift 2
            ;;
        -s | --sum)
            echo "Summing result..."
            sum dice_roll[@] final_sum
            echo "$final_sum"
            shift
            ;;
        -m | --median)
            echo "Calculating median..."
            echo "$(median amount expectation)"
            shift
            ;;
        --) shift; 
            break 
            ;;
    esac
done
