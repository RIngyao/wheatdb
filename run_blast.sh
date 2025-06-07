#!/usr/bin/env bash

# run the blast

source /home/user1/miniconda3/etc/profile.d/conda.sh 
conda activate blast

#parameters required
query=$1
e_val=$2
word_size_val=$3
gap_op_val=$4
gap_ext_val=$5
reward_val=$6
penalty_val=$7
max_target_val=$8
blast_db="data-raw/blast_db/blast_db"
outputfile="data-raw/blast_result"


blastn -query $query \
       -evalue $e_val \
       -word_size $word_size_val \
       -gapopen $gap_op_val \
       -gapextend $gap_ext_val \
        -reward $reward_val \
	-penalty $penalty_val \
	-max_target_seqs $max_target_val \
	-db $blast_db \
	-out $outputfile \
	-num_threads 25 \
        -outfmt 6 >"data-raw/blast.log" 2>&1  	
	


#blastcmd <- sprintf("blastn -query %s -evalue %f -word_size %.0f -gapopen %.0f -gapextend %.0f -penalty %.0f -reward %.0f -max_target_seqs %.0f -db %s -outfmt 6", query, e_val, word_size_val, gap_op_val, gap_ext_val, penalty_val, reward_val, max_target_val, blast_db)

