use std::env;
use std::fs;
use std::iter;

fn solve_part_one(left_list: &Vec<i32>, right_list: &Vec<i32>) -> i32 {
    let mut left_list = left_list.clone();
    let mut right_list = right_list.clone();
    left_list.sort();
    right_list.sort();

    let answer = iter::zip(left_list, right_list)
        .map(|(left_num, right_num)| (left_num - right_num).abs())
        .sum();

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

    println!("--- Day 1: Historian Hysteria ---");
    println!(
        "Answer for part 1: {}",
        solve_part_one(&left_list, &right_list)
    );
}
