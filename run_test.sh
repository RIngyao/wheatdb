#!/usr/bin/env bash

# run the blast

source /home/user1/miniconda3/etc/profile.d/conda.sh 
conda activate blast

#parameters required
query=$1
e_val=$2
blast_db="../ref_data/blast_db/blast_db"
outputfile="blast_result"


blastn -query $query \
       -evalue $e_val \
	-db $blast_db \
	-out $outputfile 
	

#blast command 

#blastcmd <- sprintf("blastn -query %s -evalue %f -word_size %.0f -gapopen %.0f -gapextend %.0f -penalty %.0f -reward %.0f -max_target_seqs %.0f -db %s -outfmt 6", query, e_val, word_size_val, gap_op_val, gap_ext_val, penalty_val, reward_val, max_target_val, blast_db)

#blast result
 # blast_res <- vroom::vroom("../wheatdb/blast_result.out", delim = "\t", col_names = FALSE)
