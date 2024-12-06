#include <cstdlib>
#include <fstream>
#include <iostream>
#include <vector>

using namespace std;

vector<string> readlines(const char *input_file) {
    vector<string> lines;
    ifstream input{input_file};

    string line;
    while (getline(input, line)) {
        lines.push_back(line);
    }

    return lines;
}

int count_all_xmas(const string &str) {
    int count = 0;

    size_t pos = str.find("XMAS");
    while (pos != string::npos) {
        count++;
        pos = str.find("XMAS", pos + 4);
    }

    pos = str.find("SAMX");
    while (pos != string::npos) {
        count++;
        pos = str.find("SAMX", pos + 4);
    }

    return count;
}

int solve_part_one(const vector<string> &wordsearch) {
    int answer = 0;

    size_t num_rows = wordsearch.size();
    size_t num_columns = wordsearch[0].length();

    vector<string> columns(num_columns);
    vector<string> diagonals_right(num_rows + num_columns - 1);
    vector<string> diagonals_left(num_rows + num_columns - 1);

    for (size_t y = 0; y < num_rows; y++) {
        for (size_t x = 0; x < num_columns; x++) {
            columns[x].push_back(wordsearch[y][x]);

            size_t diag_right_index = y + x;
            diagonals_right[diag_right_index].push_back(wordsearch[y][x]);

            size_t diag_left_index = y - x + (num_columns - 1);
            diagonals_left[diag_left_index].push_back(wordsearch[y][x]);
        }
        answer += count_all_xmas(wordsearch[y]);
    }

    for (const auto &column : columns) {
        answer += count_all_xmas(column);
    }

    for (const auto &diagonal : diagonals_right) {
        answer += count_all_xmas(diagonal);
    }

    for (const auto &diagonal : diagonals_left) {
        answer += count_all_xmas(diagonal);
    }

    return answer;
}

bool is_x_mas(
    const vector<string> &wordsearch, const size_t &y, const size_t &x
) {
    char center = wordsearch[y + 1][x + 1];
    if (center != 'A') {
        return false;
    }

    char top_left = wordsearch[y][x];
    char top_right = wordsearch[y][x + 2];
    char bottom_left = wordsearch[y + 2][x];
    char bottom_right = wordsearch[y + 2][x + 2];

    if (top_left == 'M' && bottom_right == 'S') {
        if (top_right == 'M' && bottom_left == 'S') {
            return true;
        }
        if (top_right == 'S' && bottom_left == 'M') {
            return true;
        }
    }

    if (top_left == 'S' && bottom_right == 'M') {
        if (top_right == 'M' && bottom_left == 'S') {
            return true;
        }
        if (top_right == 'S' && bottom_left == 'M') {
            return true;
        }
    }

    return false;
}

int solve_part_two(const vector<string> &wordsearch) {
    int answer = 0;

    for (size_t y = 0; y < wordsearch.size() - 2; y++) {
        for (size_t x = 0; x < wordsearch[0].length() - 2; x++) {
            if (is_x_mas(wordsearch, y, x)) {
                answer += 1;
            }
        }
    }

    return answer;
}

int main(int argc, char *argv[]) {
    if (argc < 2) {
        cerr << "Input file not provided" << endl;
        return EXIT_FAILURE;
    }

    vector<string> wordsearch = readlines(argv[1]);

    cout << "--- Day 4: Ceres Search ---" << endl;
    cout << "Answer for part 1: " << solve_part_one(wordsearch) << endl;
    cout << "Answer for part 2: " << solve_part_two(wordsearch) << endl;
}
