.onLoad = function(libname, pkgname) {
    #hackish method to set up a custom distribution for use with map2stan
    rethinking_env = loadNamespace("rethinking")
    unlockBinding("map2stan.templates", rethinking_env)
    rethinking_env$map2stan.templates$DiscreteTruncatedStudentT =
        list(
            # not built into Stan, but can build from log_prob calculations
            # need to flag by using 'increment_log_prob' as distribution name
            # then provide separate model{} and gq{} code segments
            name = "DiscreteTruncatedStudentT",
            R_name = "ddtstudent",
            stan_name = "increment_log_prob",
            stan_code = "target +=
            //interval censoring: responses were rounded to intervals of size PAR4
            log_diff_exp(
            student_t_lcdf(OUTCOME + PAR4/2.0  | PAR1, PAR2, PAR3),
            student_t_lcdf(OUTCOME - PAR4/2.0  | PAR1, PAR2, PAR3)
            ) -
            //truncation: only responses between PAR5 and PAR6 were allowed
            log_diff_exp(
            student_t_lcdf(PAR6 + PAR4/2.0  | PAR1, PAR2, PAR3),
            student_t_lcdf(PAR5 - PAR4/2.0  | PAR1, PAR2, PAR3)
            );",
        stan_dev = "dev = dev + (-2)*(
            //interval censoring: responses were rounded to intervals of size PAR4
            log_diff_exp(
            student_t_lcdf(OUTCOME + PAR4/2.0  | PAR1, PAR2, PAR3),
            student_t_lcdf(OUTCOME - PAR4/2.0  | PAR1, PAR2, PAR3)
            ) -
            //truncation: only responses between PAR5 and PAR6 were allowed
            log_diff_exp(
            student_t_lcdf(PAR6 + PAR4/2.0  | PAR1, PAR2, PAR3),
            student_t_lcdf(PAR5 - PAR4/2.0  | PAR1, PAR2, PAR3)
            ));",
        num_pars = 6,
            par_names = c("nu","mu","sigma","width","lower","upper"),
            par_bounds = c("<lower=3>","","<lower=0>","<lower=0>","",""),
            par_types = c("real","real","real","real","real","real"),
            out_type = "real",
            par_map = function(k,e,...) {
                # get constraints and add <lower=0> for width, sigma, and nu
                constr_list <- get( "constraints" , envir=e )
                width_name <- as.character( k[[4]] )
                if ( is.null(constr_list[[width_name]]) ) {
                    constr_list[[width_name]] <- "lower=0"
                    assign( "constraints" , constr_list , envir=e )
                }
                nu_name <- as.character( k[[1]] )
                if ( is.null(constr_list[[nu_name]]) ) {
                    constr_list[[nu_name]] <- "lower=3"
                    assign( "constraints" , constr_list , envir=e )
                }
                sigma_name <- as.character( k[[3]] )
                if ( is.null(constr_list[[sigma_name]]) ) {
                    constr_list[[sigma_name]] <- "lower=0"
                    assign( "constraints" , constr_list , envir=e )
                }
                return(k);
            },
            vectorized = FALSE
        )
}
