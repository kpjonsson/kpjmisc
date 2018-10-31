#' Plot Kaplan-Meier plot
#'
#' From a \code{survfit}, plot stratified Kaplan-Meier plot.
#'
#' @param survfit_object Input \code{survfit} object
#' @param plot_title Title of plot
#' @param legend_title Title of plot legend
#' @param colors Colors for strata
#' @param xmax Limit for x axis
#' @param parse_strata Convert 0/1 notation to wildtype/mutated
#' @param x_lab Label x axis
#' @param y_lab Label y axis
#' @param legend_pos Figure legend position
#' @param pval Show p-value of model
#' @param pval_pos P-value position
#' @param interval Tick interval on x axis
#' @param env_theme Theme, if not current
#' @param table Show at-risk table beneath plot (experimental)
#'
#' @return \code{ggplot2}-based Kaplan-Meier plot
#'
#' @importFrom broom tidy
#' @importFrom plyr mapvalues
#' @importFrom RColorBrewer brewer.pal
#'
#' @name plot_survival
NULL

#' @export
#' @rdname plot_survival
parse_survfit = function(survfit_object) {

	# Tidy data frame
	sf = tidy(survfit_object)
	if (!'strata' %in% names(sf)) sf$strata = ''

	# Add start at 0, by stratum
	top_rows = ldply(unique(sf$strata), function(s) {
		sf_s = filter(sf, strata == s)
		c(strata = s, time = 0, n.risk = max(sf_s$n.risk),
			n.event = 0, n.censor = 0, estimate = 1, conf.high = 1, conf.low = 1)
		})

	rbind.fill(sf, top_rows) %>%
		mutate_at(vars(estimate, time), funs(as.numeric(.))) %>%
	    arrange(strata, time, desc(estimate))
}

#' @export
#' @rdname plot_survival
plot_survfit = function(
	survfit_object,
	colors = NULL,
	legend_title = NULL,
	plot_title = NULL,
	xmax = NULL,
	parse_strata = FALSE,
	x_lab = NULL,
	y_lab = NULL,
	legend_pos = NULL,
	pval = TRUE,
	pval_pos = NULL,
	interval = NULL,
	env_theme = TRUE,
	table = FALSE
	) {

    sf = parse_survfit(survfit_object)

	if (is.null(legend_title)) {
		# legend_title = stringi::stri_trans_totitle(str_extract(names(survfit_object$strata), '^.*(?=\\=)')[1])
		legend_title = ''
	}
	if (length(survfit_object$strata)>1) {
		# s_labels = str_c(str_extract(names(survfit_object$strata), '(?<==).*'), ' (n=', survfit_object$n, ')')
	    s_labels = str_c(str_extract(names(survfit_object$strata), '(?<==).*'))
		if (parse_strata) s_labels = str_replace(s_labels, c('FALSE|0','TRUE|1'), c('WT','Mut'))
		sf = mutate(sf, strata = mapvalues(strata, names(survfit_object$strata), s_labels))
		names(survfit_object$strata) = str_extract(names(survfit_object$strata), '(?<==).*')
	} else {
		sf$strata = ''
		s_labels = str_c('n=', survfit_object$n)
		legend_pos = 'none'
	}

	if (is.null(x_lab)) x_lab = 'Time'
	if (is.null(y_lab)) y_lab = 'Survival function'
	if (is.null(xmax)) {
	    xmax = 1.04*max(sf$time)
	} else {
	    sf = filter(sf, time <= xmax) %>%
	        add_row(strata = unique(sf$strata), time = xmax) %>%
	        arrange(strata, time) %>%
	        tidyr::fill(n.risk, n.event, n.censor, estimate) %>%
	        group_by(strata) %>%
	        filter(
	            !( (time == xmax & n.risk == n.event) & (lag(n.risk) == lag(n.event) )) &
	            !(time == xmax & lag(n.risk) == 1 & lag(n.censor) == 1)) %>%
	        ungroup
	}
    legend_just = legend_pos

	sf_plot = ggplot(data = sf) +
		geom_step(aes(x = time, y = estimate, group = strata, color = strata), direction = 'hv') +
		geom_point(data = filter(sf, n.censor == 1), aes(x = time, y = estimate, color = strata), shape= '|',
			show.legend = F) +
	    scale_y_continuous(expand = c(0,0), limits = c(-.01,1.04),
	                       breaks = seq(0, 1, .25), labels = function(x) 100*x) +
	    scale_color_manual(values = brewer.pal(8, 'Paired'),
	                       labels = function(x) {
	                           str_c(x, ' (n=', survfit_object$n[match(x, names(survfit_object$strata))], ')')
	                       }) +
		theme(line = element_line(lineend = 'round'),
		      legend.position = c(1,1), legend.justification = c(1,1),
		      legend.title = element_text(size = theme_get()$legend.text$size),
		      legend.text = element_text(size = theme_get()$legend.text$size),
		      axis.line.x = element_blank(),
		      axis.line.y = element_blank(),
		      # panel.border = element_rect(size = .75, color = 'black', linetype = 'solid', fill = NA),
		      panel.background = element_rect(fill = NA),
		      plot.title = element_text(vjust = -1),
		      aspect.ratio = 1) +
		labs(x = x_lab, y = y_lab, color = legend_title)

	if (!is.null(interval)) {
	    sf_plot = sf_plot +
	        scale_x_continuous(expand = c(.01,0), limits = c(0, xmax), breaks = seq(0, xmax, interval))
	} else {
        sf_plot = sf_plot +
            scale_x_continuous(expand = c(.01,0), limits = c(0, xmax), breaks = scales::pretty_breaks(n = 4))
	}

	# Calculate p-value
	if (pval & length(survfit_object$strata)>1) {
	    sf_call = as.character(survfit_object$call)
	    sf_call = paste0(gsub('survfit', 'survdiff', sf_call[1]), '(', sf_call[2], ',data=', sf_call[3],')')
	    sd = eval(parse(text = sf_call))
	    pval = 1 - pchisq(sd$chisq, length(sd$n) - 1)
	    if (pval <= 1e-03) { pval = scales::scientific_format(digits = 3)(pval) } else { pval = round(pval, 3) }
	    if (length(unique(sf$strata))==2) {
	        cp_call = gsub('survdiff', 'coxph', sf_call)
	        cp = eval(parse(text = cp_call))
	        hr = round(exp(cp$coefficients), 2)
	        big_n = cp$n
	        pval = str_c('N=', big_n, '\nHR=', hr, '\nP-value ', pval)
	    } else {
	    	big_n = sum(survfit_object$n)
	    	pval = str_c('N=', big_n, '\nP-value ', pval)
	    }
	    if (is.null(pval_pos)) { pval_x = 0; pval_y = 0 } else { pval_x = pval_pos[1]; pval_y = pval_pos[2] }
	    sf_plot = sf_plot + annotate('text', x = pval_x, y = pval_y, label = pval,
	                                 size = rel(2.5), vjust = -.25, hjust = 0, family = 'ArialMT')
	    if (all(legend_pos==0)) legend_pos = c(1,1); legend_just = c(1,1)
	}

	sf_plot = sf_plot +
	    theme(legend.position = legend_pos, legend.justification = legend_just,
	          legend.title = element_text(size = theme_get()$legend.text$size))

	if (!is.null(plot_title)) sf_plot = sf_plot + ggtitle(plot_title)
	if (!is.null(colors)) {
		suppressMessages({
		    sf_plot = sf_plot +
		    scale_color_manual(values = colors,
		                       labels = function(x) {
		                           str_c(x, ' (n=', survfit_object$n[match(x, names(survfit_object$strata))], ')')
		                           })
		})
	} else { sf_plot }
	if (env_theme) {
	    sf_plot = sf_plot + theme_get() + theme(aspect.ratio = 1,
			panel.border = element_rect(size = theme_get()$line$size, color = 'black', linetype = 'solid', fill = NA),
			legend.position = legend_pos, legend.justification = legend_just) +
	        guides(color = guide_legend(override.aes = list(size = 1)))
	}

	if (table) {
	    table_intervals = ggplot_build(sf_plot)$layout$panel_params[[1]]$x.major_source

	    risk_tbl = map_dfr(table_intervals, function(y) {
	        group_by(sf, strata) %>%
	            group_by(strata) %>%
	            slice(which(time>y)) %>%
	            filter(time == min(time)) %>%
	            mutate(interval = y)
	    }) %>%
	        bind_rows(., filter(sf, time == xmax) %>% mutate(interval = xmax)) %>%
	        rename(at_risk = n.risk) %>%
	        ungroup() %>%
	        mutate(strata = str_trim(str_extract(risk_tbl$strata, '^[^\\(]+'), 'r'))

	    # risk_tbl = select(sf, time, strata, n.risk) %>%
	    #     mutate(interval = cut(time, breaks = table_intervals, labels = as.character(table_intervals[-1]))) %>%
	    #     arrange(strata, time) %>%
	    #     mutate(interval = ifelse(time == 0, '0', as.character(interval)),
	    #            strata = str_trim(str_extract(strata, '.*(?=\\()'), 'r')) %>%
	    #     group_by(strata, interval) %>%
	    #     summarize(at_risk = min(n.risk)) %>%
	    #     filter(!is.na(interval))

	    tbl_plot = ggplot(risk_tbl, aes(x = interval, y = strata, label = at_risk)) +
	        geom_text(family = 'ArialMT', size = 4) +
	        # scale_x_discrete(expand = c(.01, 0)) +
	        theme(axis.text.x = element_blank(),
	              axis.ticks.x = element_blank(), axis.ticks.y = element_blank(),
	              axis.line.x = element_blank(), axis.line.y = element_blank()) +
	        labs(x = '', y = '')

	    plot_grob = rbind(ggplotGrob(sf_plot), ggplotGrob(tbl_plot), size = 'first')
	    plot_grob$heights[plot_grob$layout$t[plot_grob$layout$name=='panel'][2]] = unit(.25 , 'null')

	    grid.newpage()
	    grid.draw(plot_grob)

	} else {
	    sf_plot
    }
}

# plot_forest = function(coxm, parse_var = F, env_theme = T) {
#
# 	if(env_theme) theme_set(theme_get())
#
# 	tidy_coxm = broom::tidy(coxm, exponentiate = T)
# 	cox_call = as.character(coxm$call)
# 	time = str_trim(str_extract(cox_call[2], '(?<=Surv\\().*(?=,)'), 'both')
# 	event = str_trim(str_extract(cox_call[2], '(?<=,).*(?=\\))'), 'both')
# 	vars = str_trim(str_extract(cox_call[2], '(?<=~).*'), 'both')
#
# 	df = eval(parse(text = cox_call[3]))
# 	if (vars == '.') {
# 		df_vars = select(df, -matches(paste0(time, '|', event)))
# 	} else {
# 		df_vars = select(df, matches(vars))
# 	}
#
# 	inf_error = filter(tidy_coxm, conf.high==Inf)
# 	tidy_coxm = mutate(tidy_coxm, conf.high = ifelse(conf.high==Inf,0,conf.high))
# 	if(nrow(inf_error)>0) {
# 		inf_gene = str_replace_all(inf_error$term, 'Mut', '')
# 		cat('Warning: ', paste0(inf_gene, collapse = ', '), ' has/have Inf CI')
# 	}
#
#
# 	df_vars_tbl = mutate(df_vars, row = row_number()) %>%
# 		melt(id.vars = 'row') %>%
# 		count(variable, value) %>%
# 		ungroup() %>%
# 		mutate(variable = as.character(variable))
#
# 	tidy_coxm = mutate(tidy_coxm, value = str_extract_all(term, paste0(unique(df_vars_tbl$value), collapse = '|')),
# 		term = str_replace(term, paste0(unique(df_vars_tbl$value), collapse = '|'), ''),
# 			value = as.character(value),
# 			term = as.character(term))
#
# 	if (parse_var) {
#
# 		df_vars_tbl = left_join(df_vars_tbl, tidy_coxm, by = c('variable' = 'term', 'value')) %>%
# 			replace_na(list(estimate = 1)) %>%
# 			mutate(p.value = ifelse(p.value < 1e-03, scales::scientific_format(digits = 3)(p.value), round(p.value, 3)),
# 				hr_print = str_c(round(estimate, 2), ' (', round(conf.low, 2),',', round(conf.high, 2), ')'),
# 				variable = factor(variable, ordered = T),
# 				value = factor(value, c('WT','Mut', ordered = T))) %>%
# 			arrange(variable, value)
#
# 		x_max = max(df_vars_tbl$conf.high, na.rm = T)
# 		x_min = min(df_vars_tbl$conf.low, na.rm = T)
#
# 		terms = as.character(df_vars_tbl$variable)
# 		terms[c(FALSE,TRUE)] = ''
# 		multipl = length(unique(df_vars_tbl$variable))*length(unique(df_vars_tbl$value))
#
# 		a_labs = data.frame(var = c(terms, str_c(as.character(df_vars_tbl$value), ' ', df_vars_tbl$n)),
# 			x = c(rep('Term', multipl), rep('N', multipl)),
# 			y = rep(1:multipl, 2))
# 		a_labs$x = factor(a_labs$x, c('Term','N'), ordered = T)
# 		# if (nrow(a_labs)%%3==0) { a_labs$col = rep(c(alpha('lightgrey', .5),alpha('lightgrey', .5),'white','white'), multipl/2)
# 		# } else {
# 			# a_labs$col = c(alpha('lightgrey', .5),alpha('lightgrey', .5),'white','white',alpha('lightgrey', .5),alpha('lightgrey', .5))
# 			# rep(c(alpha('lightgrey', .5),alpha('lightgrey', .5),'white','white'), multipl/2),2)
# 			a_labs$col = 'white'
# 			a_labs$col[sort(c(seq(1,multipl,4),seq(1+1,multipl+1,4)))] = alpha('lightgrey', .5)
# 			a_labs$col[sort(c(seq(1+multipl,2*multipl,4),seq(2+multipl,2*multipl+1,4)))] = alpha('lightgrey', .5)
# 		# }
#
# 		table_left = ggplot(a_labs, aes(x = x, y = y, label = var)) +
# 		 	geom_tile(col = NA, fill = a_labs$col) +
# 		 	geom_hline(yintercept = max(a_labs$y)+.5, color = 'black', size = .5) +
# 			geom_text(hjust = c(rep(0, multipl), rep(1, multipl)), size = 2) +
# 			scale_y_continuous(trans = 'reverse', expand = c(0,0)) +
# 			scale_x_discrete(position = 'top', expand = c(-.2,0)) +
# 			theme(#axis.line.x = element_blank(),
# 				axis.line.y = element_blank(),
# 				axis.text.y = element_blank(),
# 				axis.ticks.y = element_blank(),
# 				axis.ticks.x = element_blank(),
# 				axis.text.x = element_text(face = 'bold', color = 'black', hjust = c(0, 1), size = 6),
# 				plot.margin = unit(c(.25,-.5,.25,.25), 'lines')) +
# 			labs(x = '', y = '')
#
# 		df_vars_tbl = mutate(df_vars_tbl, y = row_number(), y = forcats::fct_rev(as.factor(y)))
# 		# df_vars_tbl$col = rep(c(alpha('lightgrey', .5),alpha('lightgrey', .5),'white','white'), length(levels(df_vars_tbl$variable))/2)
#
# 		df_vars_tbl = group_by(df_vars_tbl, variable) %>%
# 			mutate(group = as.numeric(variable), col = ifelse(group%%2==0,'white',alpha('lightgrey', .5)))
#
# 		hr_ranges = ggplot(df_vars_tbl, aes(y = factor(y), x = estimate, width = Inf)) +
# 			geom_tile(col = NA, fill = df_vars_tbl$col) +
# 			geom_hline(yintercept = .5, color = 'black', size = .5) +
# 			geom_vline(xintercept = 1, linetype = 'dashed', size = .25, color = 'black') +
# 		    geom_point(shape = 18, size = 2.5) +
# 		    geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = .25, size = .25) +
# 		    scale_x_continuous(position = 'top', limits = c(x_min, x_max), breaks = scales::pretty_breaks()) +
# 		    scale_y_discrete(expand = c(0,0)) +
# 		    labs(x = 'Hazard ratio', y = '') +
# 		    theme(axis.text.y = element_blank(),
# 		    	axis.text.x = element_text(color = 'black'),
# 		    	axis.ticks.y = element_blank(),
# 		    	#axis.line.x = element_blank(),
# 		    	axis.line.y = element_blank(),
# 		    	axis.title.x = element_text(face = 'bold', color = 'black', size = 6),
# 		    	plot.margin = unit(c(.25,-.5,.25,-.5), 'lines'))
#
# 		b_labs = data.frame(var = c(df_vars_tbl$hr_print, df_vars_tbl$p.value),
# 			x = c(rep('', multipl), rep('P-value', multipl)),
# 			y = rep(1:multipl, 2))
# 		b_labs$x = factor(b_labs$x, c('','P-value'), ordered = T)
# 		# if (nrow(b_labs)%%3==0) { b_labs$col = rep(c(alpha('lightgrey', .5),alpha('lightgrey', .5),'white','white'), multipl/2)
# 		# } else {
# 			b_labs$col = 'white'
# 			b_labs$col[sort(c(seq(1,multipl,4),seq(1+1,multipl+1,4)))] = alpha('lightgrey', .5)
# 			b_labs$col[sort(c(seq(1+multipl,2*multipl,4),seq(2+multipl,2*multipl+1,4)))] = alpha('lightgrey', .5)
# 		# }
#
# 		table_right = ggplot(b_labs, aes(x = x, y = y, label = var)) +
# 		 	geom_tile(col = NA, fill = b_labs$col) +
# 			geom_hline(yintercept = max(b_labs$y)+.5, color = 'black', size = .5) +
# 			geom_text(hjust = c(rep(0, multipl), rep(1, multipl)), size = 2) +
# 			scale_y_continuous(trans = 'reverse', expand = c(0,0)) +
# 			scale_x_discrete(position = 'top', expand = c(-.2,0)) +
# 			theme(#axis.line.x = element_blank(),
# 				axis.line.y = element_blank(),
# 				axis.text.y = element_blank(),
# 				axis.ticks.y = element_blank(),
# 				axis.ticks.x = element_blank(),
# 				axis.text.x = element_text(face = 'bold', color = 'black', hjust = c(0, 1), size = 6),
# 				plot.margin = unit(c(.25,.25,.25,-.5), 'lines')) +
# 			labs(x = '', y = '')
#
# 		egg::ggarrange(table_left, hr_ranges, table_right, nrow = 1, widths = c(.25,.75,.25), newpage = F)
#
# 	} else {
#
# 		tidy_coxm = mutate(tidy_coxm, p.value = ifelse(p.value < 1e-03, scales::scientific_format(digits = 3)(p.value), round(p.value, 3)),
# 			hr_print = str_c(round(estimate, 2), ' (', round(conf.low, 2),',', round(conf.high, 2), ')'))
#
# 		x_max = max(tidy_coxm$conf.high, na.rm = T)
# 		x_min = min(tidy_coxm$conf.low, na.rm = T)
# 		if (x_min > 1) x_min = 0.9
#
# 		multipl = length(tidy_coxm$term)
#
# 		a_labs = data.frame(var = tidy_coxm$term,
# 			x = rep('Term',multipl),
# 			y = 1:multipl)
# 		a_labs$col = 'white'
# 		if (multipl>1) a_labs$col = rep(c(alpha('lightgrey', .5),'white'), multipl)
#
# 		table_left = ggplot(a_labs, aes(x = x, y = y, label = var)) +
# 		 	geom_tile(col = NA, fill = a_labs$col) +
# 		 	geom_hline(yintercept = max(a_labs$y)+.5, color = 'black', size = .5) +
# 			geom_text(hjust = 0.5) +
# 			scale_y_continuous(trans = 'reverse', expand = c(0,0)) +
# 			scale_x_discrete(position = 'top', expand = c(-.2,0)) +
# 			theme(axis.line.x = element_line(size = .5),
# 				axis.line.y = element_blank(),
# 				axis.text.y = element_blank(),
# 				axis.ticks.y = element_blank(),
# 				axis.ticks.x = element_blank(),
# 				axis.text.x = element_text(face = 'bold', color = 'black', hjust = c(0, 1), size = 6),
# 				plot.margin = unit(c(.25,-.5,.25,.25), 'lines')) +
# 			labs(x = '', y = '')
#
# 		tidy_coxm$col = 'white'
# 		if (multipl>1) tidy_coxm$col = rep(c(alpha('lightgrey', .5),'white'), multipl)
#
# 		hr_ranges = ggplot(tidy_coxm, aes(y = term, x = estimate)) +
# 			geom_tile(aes(x = x_max), col = NA, fill = tidy_coxm$col) +
# 			geom_hline(yintercept = .5, color = 'black', size = .5) +
# 			geom_vline(xintercept = 1, linetype = 'dashed', size = .5, color = 'black') +
# 		    geom_point(shape = 18, size = 3) +
# 		    geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = .25, size = .5) +
# 		    scale_x_continuous(position = 'top', limits = c(x_min, x_max), breaks = scales::pretty_breaks()) +
# 		    scale_y_discrete(expand = c(0,0)) +
# 		    labs(x = 'Hazard ratio', y = '') +
# 		    theme(axis.text.y = element_blank(),
# 		    	axis.text.x = element_text(color = 'black'),
# 		    	axis.ticks.y = element_blank(),
# 		    	axis.line.x = element_line(size = .5),
# 		    	axis.line.y = element_blank(),
# 		    	axis.title.x = element_text(face = 'bold', color = 'black', size = 6),
# 		    	plot.margin = unit(c(.25,-.5,.25,-.5), 'lines'))
#
# 		b_labs = data.frame(var = c(tidy_coxm$hr_print, tidy_coxm$p.value),
# 			x = c(rep('', multipl), rep('P-value', multipl)),
# 			y = rep(1:multipl, 2))
# 		b_labs$x = factor(b_labs$x, c('','P-value'), ordered = T)
# 		b_labs$col = 'white'
# 		if (multipl>1) b_labs$col = rep(c(alpha('lightgrey', .5),'white'), multipl)
#
# 		table_right = ggplot(b_labs, aes(x = x, y = y, label = var)) +
# 		 	geom_tile(col = NA, fill = b_labs$col) +
# 			geom_hline(yintercept = max(b_labs$y)+.5, color = 'black', size = .5) +
# 			geom_text(hjust = c(rep(0, multipl), rep(1, multipl))) +
# 			scale_y_continuous(trans = 'reverse', expand = c(0,0)) +
# 			scale_x_discrete(position = 'top', expand = c(-.2,0)) +
# 			theme(#axis.line.x = element_blank(),
# 				axis.line.y = element_blank(),
# 				axis.line.x = element_line(size = .5),
# 				axis.text.y = element_blank(),
# 				axis.ticks.y = element_blank(),
# 				axis.ticks.x = element_blank(),
# 				axis.text.x = element_text(face = 'bold', color = 'black', hjust = c(0, 1), size = 6),
# 				plot.margin = unit(c(.25,.25,.25,-.5), 'lines')) +
# 			labs(x = '', y = '')
#
# 		egg::ggarrange(table_left, hr_ranges, table_right, nrow = 1, widths = c(2,2,1), newpage = F)
# 	}
# }
