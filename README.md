CPU Usage Monitor

This is a simple Haskell program that monitors the CPU usage on a Linux system by parsing the /proc/stat file. It calculates the percentage of CPU usage for each CPU and for the overall system.


How It Works

The program defines a data type called CpuStats to hold the CPU statistics parsed from /proc/stat. It then defines parsers to parse the integer values from each line of the file.

The cpuStats parser is used to parse all the CPU statistics for a single CPU, while the allCpuStats parser is used to parse all the CPU statistics for all CPUs.

The cpuUsage function calculates the percentage of CPU usage for a single CPU, while the overallCpuUsage function calculates the overall percentage of CPU usage for all CPUs.

The parseProcStat function reads and parses the /proc/stat file, while the main function uses the parseProcStat function to get the CPU statistics before and after a short interval, calculates the CPU usage percentages, and displays them on the screen.
