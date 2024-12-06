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

int main(int argc, char *argv[]) {
    if (argc < 2) {
        cerr << "Input file not provided" << endl;
        return EXIT_FAILURE;
    }

    vector<string> wordsearch = readlines(argv[1]);

    cout << "--- Day 4: Ceres Search ---" << endl;
    cout << "Answer for part 1: " << solve_part_one(wordsearch) << endl;
}
