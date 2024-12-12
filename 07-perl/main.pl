use strict;
use warnings;

sub read_lines {
    my ($filename) = @_;

    open(FILE, "<" , $filename) || die "Could not open '$filename': $!\n";
    my @lines = <FILE>;
    close(FILE);

    chomp @lines;

    return @lines;
}

sub equation_can_be_true {
    my ($total, $index, $test_value, $numbers) = @_;

    return 0 if $total > $test_value;

    if ($index == @$numbers - 1) {
        return ($total + $numbers->[$index] == $test_value) ||
               ($total * $numbers->[$index] == $test_value);
    }

    my $add_result = equation_can_be_true(
        $total + $numbers->[$index], $index + 1, $test_value, $numbers
    );
    my $multiply_result = equation_can_be_true(
        $total * $numbers->[$index], $index + 1, $test_value, $numbers
    );

    return $add_result || $multiply_result;
}

sub solve_part_one {
    my @input = @_;

    my $answer = 0;

    for my $line (@input) {
        my ($test_value, @numbers) = $line =~ m/(\d+)/g;
        if (equation_can_be_true($numbers[0], 1, $test_value, \@numbers)) {
            $answer += $test_value;
        }
    }

    return $answer;
}

sub main {
    if (@ARGV < 1) {
        print STDERR "Input file not provided\n";
        exit 1;
    }

    my $input_file = $ARGV[0];
    my @input = read_lines($input_file);

    print "--- Day 7: Bridge Repair ---\n";
    printf "Answer for part 1: %d\n", solve_part_one(@input);
}

main();
