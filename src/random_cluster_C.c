/* $B%/%i%9%?!<=jB0>pJs$N=i4|CM$rM?$($k(B */
//$B%/%i%9%?!<?t$O$H$j$"$($:(B2~13$B$^$GBP1~(B

#include <R.h>
#include <Rinternals.h>
#include <math.h>
#include <stdlib.h>
#include <time.h>

struct FIRST_INFO{
	int N_VAR;
	int N_CLUST;
	int N_IND;
	int MIN_CLUST_IN;
};

int xrand(int lower, int upper);
void f_in_random_cluster(int *population, char *zero_cluster, struct FIRST_INFO info, int N, int present_ind);
int draw_rand(int *target, int N);
void f_clust_in_adj(int *population, struct FIRST_INFO info);


SEXP random_cluster_C(SEXP INFO)
{
	int N_VAR = (int) REAL(INFO)[0];
	int N_CLUST = (int) REAL(INFO)[1];
	int N_IND = (int) REAL(INFO)[2];
	int MIN_CLUST_IN = (int) REAL(INFO)[3];

	struct FIRST_INFO info;
	info.N_VAR = N_VAR;
	info.N_CLUST = N_CLUST;
	info.N_IND = N_IND;
	info.MIN_CLUST_IN = MIN_CLUST_IN;

	int i,j,k;
	char zero_cluster[N_CLUST];
	int zero_cluster_check;
	int target[N_CLUST-1];
	int j_in[N_CLUST-1];
	int LEFT;
	int *population;

	srand(time(NULL));

	SEXP ans;
	PROTECT(ans = allocVector(INTSXP, N_IND * N_VAR));
	population = INTEGER(ans);

	for(i = 0; i < N_IND; i++){
		LEFT = N_CLUST;
		for(j = 0; j < N_VAR - N_CLUST + 1; j++)
			population[i * N_VAR + j] = xrand(1, N_CLUST);

		switch(LEFT){
		case 13:
			/* $B;D$j(B12 */
			LEFT--;
			zero_cluster_check = 0;
			f_in_random_cluster(population, zero_cluster, info, LEFT, i);

			for(k = 0; k < N_CLUST - 1; k++)
				j_in[k] = 0;
			for(j_in[0] = 0; j_in[0] < N_CLUST - LEFT + 1; j_in[0]++)
				for(j_in[1] = 1; j_in[1] < N_CLUST - LEFT + 2; j_in[1]++)
					for(j_in[2] = 2; j_in[2] < N_CLUST - LEFT + 3; j_in[2]++)
						for(j_in[3] = 3; j_in[3] < N_CLUST - LEFT + 4; j_in[3]++)
							for(j_in[4] = 4; j_in[4] < N_CLUST - LEFT + 5; j_in[4]++)
								for(j_in[5] = 5; j_in[5] < N_CLUST - LEFT + 6; j_in[5]++)
									for(j_in[6] = 6; j_in[6] < N_CLUST - LEFT + 7; j_in[6]++)
										for(j_in[7] = 7; j_in[7] < N_CLUST - LEFT + 8; j_in[7]++)
											for(j_in[8] = 8; j_in[8] < N_CLUST - LEFT + 9; j_in[8]++)
												for(j_in[9] = 9; j_in[9] < N_CLUST - LEFT + 10; j_in[9]++)
													for(j_in[10] = 10; j_in[10] < N_CLUST - LEFT + 11; j_in[10]++)
														for(j_in[11] = 11; j_in[11] < N_CLUST - LEFT + 12; j_in[11]++)
															if(!zero_cluster_check){
																if(zero_cluster[j_in[0]] == 'Y' && zero_cluster[j_in[1]] == 'Y' && zero_cluster[j_in[2]] == 'Y' && zero_cluster[j_in[3]] == 'Y' && zero_cluster[j_in[4]] == 'Y' && zero_cluster[j_in[5]] == 'Y' && zero_cluster[j_in[6]] == 'Y' && zero_cluster[j_in[7]] == 'Y' && zero_cluster[j_in[8]] == 'Y' && zero_cluster[j_in[9]] == 'Y' && zero_cluster[j_in[10]] == 'Y' && zero_cluster[j_in[11]] == 'Y'){
																	for(j = 0; j < LEFT; j++)
																		target[j] = j_in[j];
																	population[i * N_VAR + N_VAR - LEFT] = draw_rand(target, LEFT);
																	zero_cluster_check = 1;
																}
															}
			if(!zero_cluster_check)
				population[i * N_VAR + N_VAR - LEFT] = xrand(1, N_CLUST);

		case 12:
			/* $B;D$j(B11 */
			LEFT--;
			zero_cluster_check = 0;
			f_in_random_cluster(population, zero_cluster, info, LEFT, i);

			for(k = 0; k < N_CLUST - 1; k++)
				j_in[k] = 0;
			for(j_in[0] = 0; j_in[0] < N_CLUST - LEFT + 1; j_in[0]++)
				for(j_in[1] = 1; j_in[1] < N_CLUST - LEFT + 2; j_in[1]++)
					for(j_in[2] = 2; j_in[2] < N_CLUST - LEFT + 3; j_in[2]++)
						for(j_in[3] = 3; j_in[3] < N_CLUST - LEFT + 4; j_in[3]++)
							for(j_in[4] = 4; j_in[4] < N_CLUST - LEFT + 5; j_in[4]++)
								for(j_in[5] = 5; j_in[5] < N_CLUST - LEFT + 6; j_in[5]++)
									for(j_in[6] = 6; j_in[6] < N_CLUST - LEFT + 7; j_in[6]++)
										for(j_in[7] = 7; j_in[7] < N_CLUST - LEFT + 8; j_in[7]++)
											for(j_in[8] = 8; j_in[8] < N_CLUST - LEFT + 9; j_in[8]++)
												for(j_in[9] = 9; j_in[9] < N_CLUST - LEFT + 10; j_in[9]++)
													for(j_in[10] = 10; j_in[10] < N_CLUST - LEFT + 11; j_in[10]++)
														if(!zero_cluster_check){
															if(zero_cluster[j_in[0]] == 'Y' && zero_cluster[j_in[1]] == 'Y' && zero_cluster[j_in[2]] == 'Y' && zero_cluster[j_in[3]] == 'Y' && zero_cluster[j_in[4]] == 'Y' && zero_cluster[j_in[5]] == 'Y' && zero_cluster[j_in[6]] == 'Y' && zero_cluster[j_in[7]] == 'Y' && zero_cluster[j_in[8]] == 'Y' && zero_cluster[j_in[9]] == 'Y' && zero_cluster[j_in[10]] == 'Y'){
																for(j = 0; j < LEFT; j++)
																	target[j] = j_in[j];
																population[i * N_VAR + N_VAR - LEFT] = draw_rand(target, LEFT);
																zero_cluster_check = 1;
															}
														}
			if(!zero_cluster_check)
				population[i * N_VAR + N_VAR - LEFT] = xrand(1, N_CLUST);

		case 11:
			/* $B;D$j(B10 */
			LEFT--;
			zero_cluster_check = 0;
			f_in_random_cluster(population, zero_cluster, info, LEFT, i);

			for(k = 0; k < N_CLUST - 1; k++)
				j_in[k] = 0;
			for(j_in[0] = 0; j_in[0] < N_CLUST - LEFT + 1; j_in[0]++)
				for(j_in[1] = 1; j_in[1] < N_CLUST - LEFT + 2; j_in[1]++)
					for(j_in[2] = 2; j_in[2] < N_CLUST - LEFT + 3; j_in[2]++)
						for(j_in[3] = 3; j_in[3] < N_CLUST - LEFT + 4; j_in[3]++)
							for(j_in[4] = 4; j_in[4] < N_CLUST - LEFT + 5; j_in[4]++)
								for(j_in[5] = 5; j_in[5] < N_CLUST - LEFT + 6; j_in[5]++)
									for(j_in[6] = 6; j_in[6] < N_CLUST - LEFT + 7; j_in[6]++)
										for(j_in[7] = 7; j_in[7] < N_CLUST - LEFT + 8; j_in[7]++)
											for(j_in[8] = 8; j_in[8] < N_CLUST - LEFT + 9; j_in[8]++)
												for(j_in[9] = 9; j_in[9] < N_CLUST - LEFT + 10; j_in[9]++)
													if(!zero_cluster_check){
														if(zero_cluster[j_in[0]] == 'Y' && zero_cluster[j_in[1]] == 'Y' && zero_cluster[j_in[2]] == 'Y' && zero_cluster[j_in[3]] == 'Y' && zero_cluster[j_in[4]] == 'Y' && zero_cluster[j_in[5]] == 'Y' && zero_cluster[j_in[6]] == 'Y' && zero_cluster[j_in[7]] == 'Y' && zero_cluster[j_in[8]] == 'Y' && zero_cluster[j_in[9]] == 'Y'){
															for(j = 0; j < LEFT; j++)
																target[j] = j_in[j];
															population[i * N_VAR + N_VAR - LEFT] = draw_rand(target, LEFT);
															zero_cluster_check = 1;
														}
													}
			if(!zero_cluster_check)
				population[i * N_VAR + N_VAR - LEFT] = xrand(1, N_CLUST);

		case 10:
			/* $B;D$j(B9 */
			LEFT--;
			zero_cluster_check = 0;
			f_in_random_cluster(population, zero_cluster, info, LEFT, i);

			for(k = 0; k < N_CLUST - 1; k++)
				j_in[k] = 0;
			for(j_in[0] = 0; j_in[0] < N_CLUST - LEFT + 1; j_in[0]++)
				for(j_in[1] = 1; j_in[1] < N_CLUST - LEFT + 2; j_in[1]++)
					for(j_in[2] = 2; j_in[2] < N_CLUST - LEFT + 3; j_in[2]++)
						for(j_in[3] = 3; j_in[3] < N_CLUST - LEFT + 4; j_in[3]++)
							for(j_in[4] = 4; j_in[4] < N_CLUST - LEFT + 5; j_in[4]++)
								for(j_in[5] = 5; j_in[5] < N_CLUST - LEFT + 6; j_in[5]++)
									for(j_in[6] = 6; j_in[6] < N_CLUST - LEFT + 7; j_in[6]++)
										for(j_in[7] = 7; j_in[7] < N_CLUST - LEFT + 8; j_in[7]++)
											for(j_in[8] = 8; j_in[8] < N_CLUST - LEFT + 9; j_in[8]++)
												if(!zero_cluster_check){
													if(zero_cluster[j_in[0]] == 'Y' && zero_cluster[j_in[1]] == 'Y' && zero_cluster[j_in[2]] == 'Y' && zero_cluster[j_in[3]] == 'Y' && zero_cluster[j_in[4]] == 'Y' && zero_cluster[j_in[5]] == 'Y' && zero_cluster[j_in[6]] == 'Y' && zero_cluster[j_in[7]] == 'Y' && zero_cluster[j_in[8]] == 'Y'){
														for(j = 0; j < LEFT; j++)
															target[j] = j_in[j];
														population[i * N_VAR + N_VAR - LEFT] = draw_rand(target, LEFT);
														zero_cluster_check = 1;
													}
												}
			if(!zero_cluster_check)
				population[i * N_VAR + N_VAR - LEFT] = xrand(1, N_CLUST);

		case 9:
			/* $B;D$j(B8 */
			LEFT--;
			zero_cluster_check = 0;
			f_in_random_cluster(population, zero_cluster, info, LEFT, i);

			for(k = 0; k < N_CLUST - 1; k++)
				j_in[k] = 0;
			for(j_in[0] = 0; j_in[0] < N_CLUST - LEFT + 1; j_in[0]++)
				for(j_in[1] = 1; j_in[1] < N_CLUST - LEFT + 2; j_in[1]++)
					for(j_in[2] = 2; j_in[2] < N_CLUST - LEFT + 3; j_in[2]++)
						for(j_in[3] = 3; j_in[3] < N_CLUST - LEFT + 4; j_in[3]++)
							for(j_in[4] = 4; j_in[4] < N_CLUST - LEFT + 5; j_in[4]++)
								for(j_in[5] = 5; j_in[5] < N_CLUST - LEFT + 6; j_in[5]++)
									for(j_in[6] = 6; j_in[6] < N_CLUST - LEFT + 7; j_in[6]++)
										for(j_in[7] = 7; j_in[7] < N_CLUST - LEFT + 8; j_in[7]++)
											if(!zero_cluster_check){
												if(zero_cluster[j_in[0]] == 'Y' && zero_cluster[j_in[1]] == 'Y' && zero_cluster[j_in[2]] == 'Y' && zero_cluster[j_in[3]] == 'Y' && zero_cluster[j_in[4]] == 'Y' && zero_cluster[j_in[5]] == 'Y' && zero_cluster[j_in[6]] == 'Y' && zero_cluster[j_in[7]] == 'Y'){
													for(j = 0; j < LEFT; j++)
														target[j] = j_in[j];
													population[i * N_VAR + N_VAR - LEFT] = draw_rand(target, LEFT);
													zero_cluster_check = 1;
												}
											}
			if(!zero_cluster_check)
				population[i * N_VAR + N_VAR - LEFT] = xrand(1, N_CLUST);

		case 8:
			/* $B;D$j(B7 */
			LEFT--;
			zero_cluster_check = 0;
			f_in_random_cluster(population, zero_cluster, info, LEFT, i);

			for(k = 0; k < N_CLUST - 1; k++)
				j_in[k] = 0;
			for(j_in[0] = 0; j_in[0] < N_CLUST - LEFT + 1; j_in[0]++)
				for(j_in[1] = 1; j_in[1] < N_CLUST - LEFT + 2; j_in[1]++)
					for(j_in[2] = 2; j_in[2] < N_CLUST - LEFT + 3; j_in[2]++)
						for(j_in[3] = 3; j_in[3] < N_CLUST - LEFT + 4; j_in[3]++)
							for(j_in[4] = 4; j_in[4] < N_CLUST - LEFT + 5; j_in[4]++)
								for(j_in[5] = 5; j_in[5] < N_CLUST - LEFT + 6; j_in[5]++)
									for(j_in[6] = 6; j_in[6] < N_CLUST - LEFT + 7; j_in[6]++)
										if(!zero_cluster_check){
											if(zero_cluster[j_in[0]] == 'Y' && zero_cluster[j_in[1]] == 'Y' && zero_cluster[j_in[2]] == 'Y' && zero_cluster[j_in[3]] == 'Y' && zero_cluster[j_in[4]] == 'Y' && zero_cluster[j_in[5]] == 'Y' && zero_cluster[j_in[6]] == 'Y'){
												for(j = 0; j < LEFT; j++)
													target[j] = j_in[j];
												population[i * N_VAR + N_VAR - LEFT] = draw_rand(target, LEFT);
												zero_cluster_check = 1;
											}
										}
			if(!zero_cluster_check)
				population[i * N_VAR + N_VAR - LEFT] = xrand(1, N_CLUST);

		case 7:
			/* $B;D$jO;$D(B */
			LEFT--;
			zero_cluster_check = 0;
			f_in_random_cluster(population, zero_cluster, info, LEFT, i);

			for(k = 0; k < N_CLUST - 1; k++)
				j_in[k] = 0;
			for(j_in[0] = 0; j_in[0] < N_CLUST - LEFT + 1; j_in[0]++)
				for(j_in[1] = 1; j_in[1] < N_CLUST - LEFT + 2; j_in[1]++)
					for(j_in[2] = 2; j_in[2] < N_CLUST - LEFT + 3; j_in[2]++)
						for(j_in[3] = 3; j_in[3] < N_CLUST - LEFT + 4; j_in[3]++)
							for(j_in[4] = 4; j_in[4] < N_CLUST - LEFT + 5; j_in[4]++)
								for(j_in[5] = 5; j_in[5] < N_CLUST - LEFT + 6; j_in[5]++)
									if(!zero_cluster_check){
										if(zero_cluster[j_in[0]] == 'Y' && zero_cluster[j_in[1]] == 'Y' && zero_cluster[j_in[2]] == 'Y' && zero_cluster[j_in[3]] == 'Y' && zero_cluster[j_in[4]] == 'Y' && zero_cluster[j_in[5]] == 'Y'){
											for(j = 0; j < LEFT; j++)
												target[j] = j_in[j];
											population[i * N_VAR + N_VAR - LEFT] = draw_rand(target, LEFT);
											zero_cluster_check = 1;
										}
									}
			if(!zero_cluster_check)
				population[i * N_VAR + N_VAR - LEFT] = xrand(1, N_CLUST);

		case 6:
			/* $B;D$j8^$D(B */
			LEFT--;
			zero_cluster_check = 0;
			f_in_random_cluster(population, zero_cluster, info, LEFT, i);

			for(k = 0; k < N_CLUST - 1; k++)
				j_in[k] = 0;
			for(j_in[0] = 0; j_in[0] < N_CLUST - LEFT + 1; j_in[0]++)
				for(j_in[1] = 1; j_in[1] < N_CLUST - LEFT + 2; j_in[1]++)
					for(j_in[2] = 2; j_in[2] < N_CLUST - LEFT + 3; j_in[2]++)
						for(j_in[3] = 3; j_in[3] < N_CLUST - LEFT + 4; j_in[3]++)
							for(j_in[4] = 4; j_in[4] < N_CLUST - LEFT + 5; j_in[4]++)
								if(!zero_cluster_check){
									if(zero_cluster[j_in[0]] == 'Y' && zero_cluster[j_in[1]] == 'Y' && zero_cluster[j_in[2]] == 'Y' && zero_cluster[j_in[3]] == 'Y' && zero_cluster[j_in[4]] == 'Y'){
										for(j = 0; j < LEFT; j++)
											target[j] = j_in[j];
										population[i * N_VAR + N_VAR - LEFT] = draw_rand(target, LEFT);
										zero_cluster_check = 1;
									}
								}
			if(!zero_cluster_check)
				population[i * N_VAR + N_VAR - LEFT] = xrand(1, N_CLUST);

		case 5:
			/* $B;D$j;M$D(B */
			LEFT--;
			zero_cluster_check = 0;
			f_in_random_cluster(population, zero_cluster, info, LEFT, i);

			for(k = 0; k < N_CLUST - 1; k++)
				j_in[k] = 0;
			for(j_in[0] = 0; j_in[0] < N_CLUST - LEFT + 1; j_in[0]++)
				for(j_in[1] = 1; j_in[1] < N_CLUST - LEFT + 2; j_in[1]++)
					for(j_in[2] = 2; j_in[2] < N_CLUST - LEFT + 3; j_in[2]++)
						for(j_in[3] = 3; j_in[3] < N_CLUST - LEFT + 4; j_in[3]++)
							if(!zero_cluster_check){
								if(zero_cluster[j_in[0]] == 'Y' && zero_cluster[j_in[1]] == 'Y' && zero_cluster[j_in[2]] == 'Y' && zero_cluster[j_in[3]] == 'Y'){
									for(j = 0; j < LEFT; j++)
										target[j] = j_in[j];
									population[i * N_VAR + N_VAR - LEFT] = draw_rand(target, LEFT);
									zero_cluster_check = 1;
								}
							}
			if(!zero_cluster_check)
				population[i * N_VAR + N_VAR - LEFT] = xrand(1, N_CLUST);

		case 4:
			/* $B;D$j;0$D(B */
			LEFT--;
			zero_cluster_check = 0;
			f_in_random_cluster(population, zero_cluster, info, LEFT, i);

			for(k = 0; k < N_CLUST - 1; k++)
				j_in[k] = 0;
			for(j_in[0] = 0; j_in[0] < N_CLUST - LEFT + 1; j_in[0]++)
				for(j_in[1] = 1; j_in[1] < N_CLUST - LEFT + 2; j_in[1]++)
					for(j_in[2] = 2; j_in[2] < N_CLUST - LEFT + 3; j_in[2]++)
						if(!zero_cluster_check){
							if(zero_cluster[j_in[0]] == 'Y' && zero_cluster[j_in[1]] == 'Y' && zero_cluster[j_in[2]] == 'Y'){
								for(j = 0; j < LEFT; j++)
									target[j] = j_in[j];
								population[i * N_VAR + N_VAR - LEFT] = draw_rand(target, LEFT);
								zero_cluster_check = 1;
							}
						}
			if(!zero_cluster_check)
				population[i * N_VAR + N_VAR - LEFT] = xrand(1, N_CLUST);

		case 3:
			/* $B;D$jFs$D(B */
			LEFT--;
			zero_cluster_check = 0;
			f_in_random_cluster(population, zero_cluster, info, LEFT, i);

			for(k = 0; k < N_CLUST - 1; k++)
				j_in[k] = 0;
			for(j_in[0] = 0; j_in[0] < N_CLUST - LEFT + 1; j_in[0]++)
				for(j_in[1] = 1; j_in[1] < N_CLUST - LEFT + 2; j_in[1]++)
					if(!zero_cluster_check){
						if(zero_cluster[j_in[0]] == 'Y' && zero_cluster[j_in[1]] == 'Y'){
							for(j = 0; j < LEFT; j++)
								target[j] = j_in[j];
							population[i * N_VAR + N_VAR - LEFT] = draw_rand(target, LEFT);
							zero_cluster_check = 1;
						}
					}
			if(!zero_cluster_check)
				population[i * N_VAR + N_VAR - LEFT] = xrand(1, N_CLUST);

		case 2:
			/* $B;D$j0l$D(B */
			LEFT--;
			zero_cluster_check = 0;
			f_in_random_cluster(population, zero_cluster, info, LEFT, i);

			for(k = 0; k < N_CLUST - 1; k++)
				j_in[k] = 0;
			for(j_in[0] = 0; j_in[0] < N_CLUST; j_in[0]++)
				if(!zero_cluster_check){
					if(zero_cluster[j_in[0]] == 'Y'){
						for(j = 0; j < LEFT; j++)
							target[j] = j_in[j];
						population[i * N_VAR + N_VAR - LEFT] = target[0];
						zero_cluster_check = 1;
					}
				}
			if(!zero_cluster_check)
				population[i * N_VAR + N_VAR - LEFT] = xrand(1, N_CLUST);
		}
	}

	f_clust_in_adj(population, info);

	UNPROTECT(1);
	return(ans);
}

int xrand(int lower, int upper)
{
	return rand() % (upper - lower + 1) + lower;
}

void f_in_random_cluster(int *population, char *zero_cluster, struct FIRST_INFO info, int N, int present_ind)
{
	int N_VAR = info.N_VAR;
	int N_CLUST = info.N_CLUST;

	int i,j,k;
	for(k = 0; k < N_CLUST; k++)
		zero_cluster[k] = 'Y';

	for(j = 0; j < N_VAR - N; j++)
		for(k = 0; k < N_CLUST; k++)
			if(population[present_ind * N_VAR + j] == k + 1)
				zero_cluster[k] = 'N';
}

int draw_rand(int *target, int N)
{
	int tmp_prob;
	tmp_prob = xrand(1, N);

	return target[tmp_prob-1];
}

void f_clust_in_adj(int *population, struct FIRST_INFO info)
{
	int N_IND = info.N_IND;
	int N_VAR = info.N_VAR;
	int N_CLUST = info.N_CLUST;
	int MIN_CLUST_IN = info.MIN_CLUST_IN;

	int i,j,k,l;
	int n_clust_in[N_CLUST], clust_pattern[N_VAR];
	int MAX_CLUST_NUM_OF_VAR, MAX_CLUST_NUM, random_num, tmp_clust;
	int out_warning;

	for(i = 0; i < N_IND; i++){
		//$B3F%/%i%9%?!<$N=jB0?t$rD4$Y$k(B
		for(k = 0; k < N_CLUST; k++)
			n_clust_in[k] = 0;
		for(k = 0; k < N_CLUST; k++)
			for(j = 0; j < N_VAR; j++)
				if(population[i * N_VAR + j] == (k+1))
					n_clust_in[k] += 1;

		for(k = 0; k < N_CLUST; k++){
			//$B3F%/%i%9%?!<$N=jB0JQ?t$N?t$,(BMIN_CLUST_IN$BL$K~$+$I$&$+(B
			while(n_clust_in[k] < MIN_CLUST_IN){
				MAX_CLUST_NUM_OF_VAR = 0;
				//$B:GBgJQ?t3JG<%/%i%9%?!<$r5a$a$k(B
				for(j = 0; j < N_CLUST; j++)
					if(MAX_CLUST_NUM_OF_VAR < n_clust_in[j]){
						MAX_CLUST_NUM_OF_VAR = n_clust_in[j];
						MAX_CLUST_NUM = j;
					}
				//$B:GBg%/%i%9%?!<$N=jB0JQ?t$r%i%s%@%`$K0l$D(Bk+1$B$KJQ99(B
				random_num = xrand(1, MAX_CLUST_NUM_OF_VAR);
				tmp_clust = 0;
				for(l = 0; l < N_VAR; l++){
					if(population[i * N_VAR + l] == (MAX_CLUST_NUM+1)){
						tmp_clust += 1;
						if(tmp_clust == random_num){
							population[i * N_VAR + l] = k + 1;
						}
					}
				}

				//$B3F%/%i%9%?!<$N=jB0?t$r99?7(B
				for(j = 0; j < N_CLUST; j++)
					n_clust_in[j] = 0;
				for(j = 0; j < N_CLUST; j++)
					for(l = 0; l < N_VAR; l++)
						if(population[i * N_VAR + l] == (j+1))
							n_clust_in[j] += 1;
			}
		}
	}
}
