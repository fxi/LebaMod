customSummary<-function (data, lev = NULL, model = NULL) {
  require(e1071)
	# avec data=le résultats du modèle, lev=les niveaux souhaité (PRES et ABS), et model=le modèle evalué.
	# fusion entre defautlSummary, twoClassSummary
	if (!all(levels(data[, "pred"]) == levels(data[, "obs"]))){
		stop("levels of observed and predicted data do not match")
	}
	# ROC sur la base des données observée et prédite

	# rocObject <- try(pROC:::roc(as.numeric(obs)~as.numeric(pred),data))
	#try(pROC::auc(obs~pred,data))
	# Aire sous la courbe ROC, seulement s'il n'y a pas d'erreur 
	# rocAUC <- if (class(rocObject)[1] == "try-error"){
	#	 NA
	#}else{
	#rocObject$auc
	#}
	# Calcul de la sensibilité et de la spécificité
	p<-data[, "pred"]
	o<-data[, "obs"]
	spec<-specificity(p, o, lev[2])
	sens<-sensitivity(p, o, lev[1])
	# Calcul de TSS (allouche2006)
	tss<-sens+spec-1
	# Calcul de Kappa avec la fonction classAgreement
	kappa <- unlist(classAgreement(table(o, p)))[c("kappa")]
	# Sortie et renommage
	#out <- c(tss, kappa, rocAUC, sens , spec )
	out <- c(tss, kappa, sens , spec )
	#names(out) <- c("TSS","Kappa","AUC", "Sens", "Spec")
	names(out) <- c("TSS","Kappa", "Sens", "Spec")
	out
}

