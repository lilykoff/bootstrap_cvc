Rnosave run_bootstrap_100.R -J BOOT100 --mem=80G --nodes=1 --ntasks=1 --cpus-per-task=8 --array=2-1000 --mail-type=FAIL,END --mail-user=lkoffma2@jh.edu -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err

Rnosave run_bootstrap_50.R -J BOOT50 --mem=60G --nodes=1 --ntasks=1 --cpus-per-task=8 --array=1-1000 --mail-type=FAIL,END --mail-user=lkoffma2@jh.edu -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err
Rnosave run_bootstrap_10.R -J BOOT10 --mem=40G --nodes=1 --ntasks=1 --cpus-per-task=8 --array=1-1000 --mail-type=FAIL,END --mail-user=lkoffma2@jh.edu -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err
Rnosave run_bootstrap_500.R -J BOOT500 --mem=100G --nodes=1 --ntasks=1 --cpus-per-task=8 --array=1-1000 --mail-type=FAIL,END --mail-user=lkoffma2@jh.edu -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err
