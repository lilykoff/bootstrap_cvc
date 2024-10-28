Rnosave run_bootstrap_100.R -J BOOT100 --mem=80G --nodes=1 --ntasks=1 --cpus-per-task=8 --array=2-1000 --mail-type=FAIL,END --mail-user=lkoffma2@jh.edu -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err

Rnosave run_bootstrap_50.R -J BOOT50 --mem=60G --nodes=1 --ntasks=1 --cpus-per-task=8 --array=1-1000 --mail-type=FAIL,END --mail-user=lkoffma2@jh.edu -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err
Rnosave run_bootstrap_10.R -J BOOT10 --mem=40G --nodes=1 --ntasks=1 --cpus-per-task=8 --array=1-1000 --mail-type=FAIL,END --mail-user=lkoffma2@jh.edu -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err
Rnosave run_bootstrap_500.R -J BOOT500 --mem=100G --nodes=1 --ntasks=1 --cpus-per-task=8 --array=1-1000 --mail-type=FAIL,END --mail-user=lkoffma2@jh.edu -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err

Rnosave known_auc_sim.R -J AUC_SIM --array=2-120 --mem=10G --mail-type=FAIL,END --mail-user=lkoffma2@jh.edu -o eofiles/%x_%a.out -e eofiles/%x_%a.err
Rnosave generate_known_auc_dfs.R -J GENAUC --mem=30G -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err

Rnosave auc_cohen_d_sim.R -J AUC_COHEND --mem=50G --cpus-per-task=8 --array=1-24 --mail-type=FAIL,END --mail-user=lkoffma2@jh.edu -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err
Rnosave auc_cohen_d_boot_sim.R -J AUC_COHEND --mem=30G --cpus-per-task=24 --array=1-24 --mail-type=FAIL,END --mail-user=lkoffma2@jh.edu -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err
Rnosave auc_cohen_d_boot_sim.R -J AUC_COHEND --mem=60G --cpus-per-task=8 --array=1-24 --time=3-00 --mail-type=FAIL,END --mail-user=lkoffma2@jh.edu -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err


Rnosave 00_generate_populations.R -J GENAUC --mem=30G -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err
Rnosave 01_generate_boot_inds.R -J GENBOOTINDS --mem=200G --mail-type=FAIL,END --mail-user=lkoffma2@jh.edu -o eofiles/%x_%j.out -e eofiles/%x_%j.err
Rnosave 02_run_models.R -J RUNMODELS --array=1 --mem=50G --mail-type=FAIL,END --mail-user=lkoffma2@jh.edu -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err

Rnosave 01_generate_boot_inds.R -J GENBOOTINDSA --array=1-1000 --mem=25G --mail-type=FAIL,END --mail-user=lkoffma2@jh.edu -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err
Rnosave 01_run_auc.R -J RUN_AUC --array=1 --mem=50G --mail-type=FAIL,END --mail-user=lkoffma2@jh.edu -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err
Rnosave 01_run_auc.R -J RUN_AUCA --array=2-1000 --mem=10G --time=7-00 --mail-type=FAIL,END --mail-user=lkoffma2@jh.edu -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err
