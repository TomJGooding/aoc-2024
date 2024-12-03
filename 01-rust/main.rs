use std::collections::HashMap;
use std::env;
use std::fs;
use std::iter;

fn solve_part_one(left_list: &Vec<i32>, right_list: &Vec<i32>) -> i32 {
    let answer = iter::zip(left_list, right_list)
        .map(|(left_num, right_num)| (left_num - right_num).abs())
        .sum();

    return answer;
}

fn solve_part_two(left_list: &Vec<i32>, right_list: &Vec<i32>) -> i32 {
    let mut number_counter = HashMap::new();
    for number in right_list {
        number_counter
            .entry(number)
            .and_modify(|counter| *counter += 1)
            .or_insert(1);
    }

    let mut answer = 0;
    for number in left_list {
        answer += number * number_counter.get(number).unwrap_or(&0);
    }

    return answer;
}

fn main() {
    let input_file = env::args().nth(1).expect("Input file not provided");
    let input = fs::read_to_string(&input_file).expect("Cannot open input file");

    let mut left_list: Vec<i32> = Vec::new();
    let mut right_list: Vec<i32> = Vec::new();
    for line in input.lines() {
        let numbers: Vec<i32> = line
            .split_whitespace()
            .filter_map(|s| s.parse::<i32>().ok())
            .collect();
        assert_eq!(numbers.len(), 2, "Expected two numbers in {line}");
        left_list.push(numbers[0]);
        right_list.push(numbers[1]);
    }

    left_list.sort();
    right_list.sort();

    println!("--- Day 1: Historian Hysteria ---");
    println!(
        "Answer for part 1: {}",
        solve_part_one(&left_list, &right_list)
    );
    println!(
        "Answer for part 2: {}",
        solve_part_two(&left_list, &right_list)
    );
}
