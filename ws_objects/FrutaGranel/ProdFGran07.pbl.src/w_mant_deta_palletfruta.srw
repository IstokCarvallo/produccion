$PBExportHeader$w_mant_deta_palletfruta.srw
forward
global type w_mant_deta_palletfruta from w_mant_detalle_csd
end type
end forward

global type w_mant_deta_palletfruta from w_mant_detalle_csd
integer width = 3406
integer height = 1732
end type
global w_mant_deta_palletfruta w_mant_deta_palletfruta

type variables
DataWindowChild  	 	dw_calibres,dw_calibrot,dw_productor ,&
							dw_embalajes,dw_variedades , dw_varirot, &
							dw_plantadesp,dw_especies,&
							dw_etiquetas,dw_condicion,dw_cliente, &
							dw_tipofrio,dw_categoria,dw_prodrot,dw_plantaorigen,&
							dw_periodofrio, idwc_Predio, idwc_Cuartel, idwc_Prediorot, idwc_Cuarterot

uo_lotesfrutagranel	iuo_Lote
uo_ProdPredio			iuo_Predio
uo_ProdCuarteles		iuo_Cuartel
uo_variedades			iuo_variedad
uo_fechaMovto			iuo_FechaMovto
uo_periodofrio			iuo_pfrio
End variables

forward prototypes
public subroutine buscaprod ()
public function boolean duplicado (string campo)
public subroutine fueradenorma (string as_columna, string as_valor)
public function boolean noexisteproductor (string ls_columna)
public function boolean existevariecab (integer as_valor)
public function boolean buscanombreembalaje (string as_embalaje)
public function boolean verificaproductor (long al_productor)
public subroutine cuentacajas ()
public function boolean noexistecalibre (string as_valor, string as_columna)
end prototypes

public subroutine buscaprod ();dw_1.ModIfy("buscaprod.border = 0")
dw_1.ModIfy("buscaprod.border = 5")

istr_busq.Argum[1] = istr_mant.Argumento[1]

OpenWithParm(w_busc_productores, istr_busq)

istr_busq	= Message.PowerObjectParm

If istr_busq.Argum[4] = "" Then
	dw_1.SetColumn("prod_codigo")
	dw_1.SetFocus()
Else
	dw_1.SetItem(il_fila, "prod_codigo", Long(istr_busq.Argum[4]))
	dw_1.SetItem(il_fila, "productores_prod_nombre", istr_busq.Argum[5])
	If istr_mant.argumento[14] = '1' Then
		NoExisteProductor(istr_busq.Argum[4])
	End If
	
	dw_1.SetColumn("pafr_calibr")
	dw_1.SetFocus()
End If

dw_1.ModIfy("buscaprod.border = 0")
dw_1.ModIfy("buscaprod.border = 6")

Return
End subroutine

public function boolean duplicado (string campo);Long		ll_fila
Integer	li_predio, li_cuartel
String		ls_client, ls_nropal, ls_especi, ls_varied, ls_embala, ls_produc, ls_condic, ls_etique, &
			ls_planta, ls_zonaco, ls_calibr
		
ls_client 		= 	istr_mant.argumento[1]
ls_nropal 	= 	istr_mant.argumento[2]
ls_especi 	= 	istr_mant.argumento[3]
ls_varied 	= 	istr_mant.argumento[4]
ls_embala 	= 	dw_1.Object.emba_codigo[il_Fila]
ls_produc 	= 	String(dw_1.GetItemNumber(il_Fila, "prod_codigo"))
ls_etique 	= 	istr_mant.argumento[9]
ls_planta 	= 	istr_mant.argumento[6]
ls_calibr 		= 	campo
li_predio		=	dw_1.Object.prbr_codpre[il_Fila]
li_cuartel		=	dw_1.Object.prcc_codigo[il_Fila]

//						    " AND paen_numero =  " + ls_nropal + 			&


ll_fila	= dw_1.Find( "clie_codigo = " + ls_client + 	&
						    " AND espe_codigo =  " + ls_especi +" AND vari_codigo =  " + ls_varied + &
							 " AND emba_codigo = '" + ls_embala + "'" + 	" AND prod_codigo =  " + ls_produc + &
							 " AND etiq_codigo =  " + ls_etique + " AND plde_codigo =  " + ls_planta + &
							 " AND prbr_codpre =  " + string(li_predio) + " AND prcc_codigo =  " + string(li_cuartel) + &
							 " AND pafr_calibr = '" + ls_calibr + "'", 1, dw_1.RowCount())
							 
If ll_fila > 0 and ll_fila <> il_fila Then
	If MessageBox("Advertencia","Calidad ya fue ingresada anteriormente, ¿Utilizar de todas formas?",Information!, YesNo!) = 2 Then
		Return True
	Else
		Return False
	End If
Else
	Return False
End If
End function

public subroutine fueradenorma (string as_columna, string as_valor);Integer	li_Especie, li_Variedad, li_Categoria, li_Cantidad
String	ls_Embalaje, ls_Calibre
DateTime	ldt_FechaEmb

li_Especie		=	dw_1.Object.espe_codigo[il_Fila]
li_Variedad		=	dw_1.Object.vari_codigo[il_Fila]
li_Categoria	=	dw_1.Object.cate_codigo[il_Fila]
ldt_FechaEmb	=	datetime(date(dw_1.Object.pafr_fecemb[il_Fila]))
ls_Embalaje		=	dw_1.Object.emba_codigo[il_Fila]
ls_Calibre		=	dw_1.Object.pafr_calibr[il_Fila]

Choose Case as_Columna
	Case "emba_codigo"
		ls_Embalaje		=	as_valor
		
	Case "pafr_fecemb"
		ldt_FechaEmb	=	DateTime(Date(as_valor))
		
	Case "pafr_calibr"
		ls_Calibre		=	as_valor
		
End Choose

If Isnull(ls_Calibre) OR Isnull(ls_Embalaje) OR Isnull(ldt_FechaEmb) Then
	Return
Else
	SELECT 	Count(emfn_calibr)
		INTO	:li_Cantidad
		FROM	dbo.spro_embafueranorma
		WHERE	espe_codigo = :li_Especie
		AND	vari_codigo	=	:li_Variedad
		AND	cate_codigo	=	:li_Categoria
		AND	emba_codigo	=	:ls_Embalaje
		AND	:ldt_FechaEmb between emfn_fecini and emfn_fecter
		AND	emfn_calibr	=	:ls_Calibre;
	
	If SQLCA.SQLCode = -1 Then
		F_ErrorBaseDatos(SQLCA, "Lectura de Tabla de Fuera de Norma")
	Else
		If li_Cantidad = 0 Then
			dw_1.SetItem(il_Fila, "pafr_estemb", 1)
		Else
			dw_1.SetItem(il_Fila, "pafr_estemb", 2)
		End If
	End If
End If
End subroutine

public function boolean noexisteproductor (string ls_columna);String	ls_nombre
Integer	li_cliente
Boolean	lb_retorna = True
Long     ll_codigo

li_cliente	 = Integer (istr_mant.argumento[1])
ll_codigo 	 = Long (ls_columna)

//If istr_mant.argumento[17] = '1'and istr_mant.argumento[24] <> '1' Then
//	If Pos(istr_mant.argumento[15], "," + String(li_codigo)) = 0 Then
//		messagebox("Atención","Productor no corresponde a Pallets originales", Exclamation!, Ok!)
//		Return lb_Retorna
//	End If
//End If

	SELECT	prod_nombre INTO :ls_nombre
		FROM	dbo.productores
		WHERE	prod_codigo = :ll_codigo ;
	
	If sqlca.SQLCode = -1 Then
		F_errorbasedatos(sqlca,"Lectura tabla Productores")
		Return lb_Retorna
		
	ElseIf sqlca.SQLCode = 100 Then
		MessageBox("Atención", "Código de Productor no ha sido creado, Ingrese otro Código.", &
		Exclamation!, OK!)
		Return lb_Retorna
	Else
		//istr_mant.argumento[13]	=	string(li_codigo)
//		dw_1.SetItem (il_fila, "prod_codigo", ll_codigo)
//		dw_1.SetItem (il_fila, "productores_prod_nombre", ls_nombre)
		lb_retorna = False
		Return lb_retorna
	End If

Return lb_retorna
End function

public function boolean existevariecab (integer as_valor);Integer	li_especie, li_variedad, li_cliente
String	ls_nombre, ls_calibre

li_cliente		= integer(istr_mant.argumento[1])
li_especie	= integer(istr_mant.argumento[3])
li_variedad	= as_valor

SELECT	vaca_calibr    INTO : ls_calibre
	FROM	dbo.variecalibre
	WHERE espe_codigo		= :li_especie  and &
			vari_codigo    = :li_variedad ;//and &
	//		seca_codigo		= : ls_secacod ;
			
If sqlca.SQLCode = -1 Then
	F_errorbasedatos(sqlca,"Lectura tabla Variecalibre")
	Return False
ElseIf sqlca.SQLCode = 100 Then
	MessageBox("Atención","Código de Variedad-Calibre no Existe.", Exclamation!, Ok!)
	Return False
End If

Return TRUE
end function

public function boolean buscanombreembalaje (string as_embalaje);String	ls_nombre
Integer li_cliente

li_cliente = dw_1.Object.clie_codigo[1]

SELECT emba_nombre  INTO	:ls_nombre
  FROM dbo.embalajesprod
 WHERE emba_codigo	= :as_embalaje
 AND	 clie_codigo 	= :li_cliente;
 
If sqlca.SQLCode = -1 Then
	F_errorbasedatos(sqlca,"Lectura tabla Embalajes")
	Return False
ElseIf sqlca.SQLCode = 100 Then
	MessageBox("Atención","Código de Embalaje no Existe para este Cliente. Ingrese Otro.", Exclamation!, Ok!)
	Return False
Else
	dw_1.SetItem(1, "emba_nombre", ls_nombre)
	Return True
End If
end function

public function boolean verificaproductor (long al_productor); Integer li_cont=0, li_cliente,li_planta
 
 li_cliente=Integer(istr_mant.argumento[1])
 li_planta=Integer(istr_mant.argumento[21])

 SELECT count (prod_codigo)
    INTO :li_cont  
    FROM dbo.prodpacking  
   WHERE ( prod_codigo = :al_productor ) AND  
         ( plde_codigo = :li_planta )
           ;
If (sqlca.sqlcode)= -1 Then
	F_errorbasedatos(sqlca,"Lectura tabla Prodpacking")
	Return FALSE
ElseIf li_cont>0 Then
	Return TRUE
Else
	MessageBox("Atención", "Código de Productor No Asignado a Packing Origen.", Exclamation!, OK!)
	Return FALSE
End If

end function

public subroutine cuentacajas ();Long	ll_fila, ll_cajas

FOR ll_fila = 1 TO (dw_1.RowCount() - 1)
	ll_cajas = ll_cajas + dw_1.Object.pafr_ccajas[ll_fila]
NEXT

istr_mant.argumento[20]	=	String(ll_cajas)
End subroutine

public function boolean noexistecalibre (string as_valor, string as_columna);String	ls_Codigo, ls_Calibr
Integer	li_Especie, li_TipoEnvase, li_Envase

dw_1.AcceptText()

li_Especie		=	Integer(istr_Mant.Argumento[3])
li_TipoEnvase	=	Integer(istr_Mant.Argumento[7])
li_Envase		=	Integer(istr_Mant.Argumento[8])
ls_Codigo   	=	Upper(as_valor)

SELECT	caen_calibr
	INTO	:ls_Calibr
	FROM  dbo.calibresenvase
	WHERE espe_codigo =	:li_especie
	AND	enva_tipoen =	:li_TipoEnvase 
	AND	enva_codigo =	:li_Envase 
	AND	caen_calibr	=	:ls_Codigo ;

If sqlca.SQLCode = -1 Then
	F_errorbasedatos(sqlca,"Lectura tabla Calibres Envase")
	Return True
ElseIf sqlca.SQLCode = 100 Then
	MessageBox("Atención", "Calidad no Asignada para este Envase, Ingrese otra.", &
					Exclamation!, OK!)
	Return True
Else
	dw_1.SetItem(il_fila, as_columna, ls_Codigo)
	Return False
End If

End function

on w_mant_deta_palletfruta.create
call super::create
end on

on w_mant_deta_palletfruta.destroy
call super::destroy
end on

event ue_recuperadatos;call super::ue_recuperadatos;ias_campo[1]	=	String(dw_1.Object.plde_origen[il_fila])
ias_campo[2]	=	String(dw_1.Object.pefr_codigo[il_fila])
ias_campo[3]	=	String(dw_1.Object.prod_codigo[il_fila])
ias_campo[4]	=	String(dw_1.Object.prod_codrot[il_fila])
ias_campo[5]	=	dw_1.Object.emba_codigo[il_fila]
ias_campo[6]	=	String(dw_1.Object.pafr_fecemb[il_fila])
ias_campo[7]	=	String(dw_1.Object.pafr_estemb[il_fila])
ias_campo[8]	=	dw_1.Object.pafr_calibr[il_fila]
ias_campo[9]	=	String(dw_1.Object.cocc_codigo[il_fila])
ias_campo[10]	=	String(dw_1.Object.pafr_ccajas[il_fila])
ias_campo[11]  =  String(dw_1.Object.vari_codigo[il_fila])
ias_campo[12]  =  dw_1.Object.frio_tipofr[il_fila]
ias_campo[13]  =  String(dw_1.Object.vari_codrot[il_fila])
ias_campo[14]  =  dw_1.Object.emba_codigo[il_fila]

If istr_Mant.Agrega Then
	dw_1.SetItem(il_fila, "prod_codigo", dw_1.GetItemNumber(dw_1.GetRow(),'prod_codigo'))
	dw_1.SetItem(il_fila, "clie_codigo", Integer(istr_mant.argumento[1]))
	dw_1.SetItem(il_fila, "paen_numero", Long(istr_mant.argumento[2]))
	dw_1.SetItem(il_fila, "espe_codigo", Integer(istr_mant.argumento[3]))
	dw_1.SetItem(il_fila, "vari_codigo", Integer(istr_mant.argumento[4]))
	
	iuo_variedad.existe(dw_1.Object.espe_codigo[il_fila],Integer(istr_mant.argumento[4]),TRUE,SQLCA)
	
	dw_1.SetItem(il_fila, "vari_codrot", iuo_variedad.varirelaci)
	dw_1.SetItem(il_fila, "pafr_varrot", iuo_variedad.varirelaci)	
	dw_1.SetItem(il_fila, "vari_nombre", istr_mant.argumento[5])
	dw_1.SetItem(il_fila, "plde_codigo", Integer(istr_mant.argumento[6]))
	dw_1.SetItem(il_fila, "etiq_codigo", Integer(istr_mant.argumento[9]))
	dw_1.SetItem(il_fila, "frio_tipofr", istr_mant.argumento[10])
	dw_1.SetItem(il_fila, "cate_codigo", Integer(istr_mant.argumento[13]))
	dw_1.SetItem(il_fila, "pafr_estemb", 1)
	dw_1.SetItem(il_fila, "pafr_ccajas", Integer(istr_mant.argumento[11]))// - Integer(istr_mant.argumento[20]))
	dw_1.SetItem(il_fila, "pefr_codigo", 1)
	
	dw_1.SetItem(il_fila, "pafr_fecemb", Date(istr_mant.argumento[91]))
	dw_1.SetItem(il_fila, "emba_codigo", istr_mant.argumento[14])
	
	If gstr_parempresa.Productor > 0 Then
		dw_1.Object.prod_codigo[il_Fila]	=	gstr_parempresa.Productor
		dw_1.Object.prod_codrot[il_Fila]	=	gstr_parempresa.Productor
		
		idwc_Predio.Retrieve(gstr_parempresa.Productor)
		idwc_Prediorot.Retrieve(gstr_parempresa.Productor) 
	End If
	
	If Integer(istr_Mant.Argumento[21]) > 0 Then
		dw_1.Object.plde_origen[il_Fila]	=	Integer(istr_Mant.Argumento[21])
	Else
		dw_1.SetItem(il_fila, "plde_origen", Integer(istr_mant.argumento[6]))
	End If
Else
	dw_1.SetItem(il_fila, "etiq_codigo", Integer(istr_mant.argumento[9]))
	dw_1.SetItem(il_fila, "cate_codigo", Integer(istr_mant.argumento[13]))
	dw_1.SetItem(il_fila, "emba_codigo", istr_mant.argumento[14])
	
End If

//If istr_Mant.Argumento[20]	= '3' OR istr_Mant.Argumento[20]	= '10' OR istr_Mant.Argumento[20] = '2'Then
//	dw_1.Object.lote_codigo.Protect				=	1
//	dw_1.Object.pafr_tipdoc.Protect				=	1
//	dw_1.Object.lote_codigo.BackGround.Color	=	RGB(192,192,192)
//	dw_1.Object.pafr_tipdoc.BackGround.Color	= 	RGB(192,192,192)
//	If istr_mant.Argumento[20] <> '3' Then
//		dw_1.Object.pafr_docrel.Protect				=	1
//		dw_1.Object.pafr_docrel.BackGround.Color	= RGB(192,192,192)	
//		dw_1.Object.pafr_docrel[il_fila]				= Long(istr_mant.argumento[23])
//	End If	
//	dw_1.Object.pafr_tipdoc[il_fila]				= 2
//End If

//If istr_Mant.Argumento[20]	= '5' Then
//	dw_1.Object.pafr_tipdoc.Protect				=	1
//	dw_1.Object.pafr_docrel.Protect				=	1
//	dw_1.Object.pafr_tipdoc.BackGround.Color	= 	RGB(192,192,192)
//	dw_1.Object.pafr_docrel.BackGround.Color	= 	RGB(192,192,192)
//End If

If istr_Mant.Argumento[20]	= '2' Then
	dw_1.Object.prbr_codpre.Protect				=	1
	dw_1.Object.prcc_codigo.Protect				=	1
	dw_1.Object.prbr_codpre.BackGround.Color	=	RGB(192,192,192)
	dw_1.Object.prcc_codigo.BackGround.Color	=	RGB(192,192,192)
End If

//If istr_Mant.Argumento[20]	= '8' Then
//	dw_1.Object.lote_codigo.Protect				=	1
//	dw_1.Object.pafr_tipdoc.Protect				=	1
//	dw_1.Object.pafr_docrel.Protect				=	1
//	dw_1.Object.lote_codigo.BackGround.Color	=	RGB(192,192,192)
//	dw_1.Object.pafr_tipdoc.BackGround.Color	= 	RGB(192,192,192)
//	dw_1.Object.pafr_docrel.BackGround.Color	= 	RGB(192,192,192)
//End If

If Not istr_mant.Agrega And Not istr_mant.Borra Then
	istr_mant.argumento[11]=String(Long(istr_mant.argumento[11])+Long(ias_campo[10]))
End If
end event

event ue_deshace;call super::ue_deshace;If UpperBound(ias_campo) > 0 Then
	dw_1.Object.plde_origen[il_fila]	=	Integer(ias_campo[1])
	dw_1.Object.pefr_codigo[il_fila]	=	Integer(ias_campo[2])
	dw_1.Object.prod_codigo[il_fila]	=	Long(ias_campo[3])
	dw_1.Object.prod_codrot[il_fila]	=	Integer(ias_campo[4])
	dw_1.Object.emba_codigo[il_fila]	=	ias_campo[5]
	dw_1.Object.pafr_fecemb[il_fila]	=	DateTime(Date(Mid(ias_campo[6],1,10)))
	dw_1.Object.pafr_estemb[il_fila]	=	Integer(ias_campo[7])
	dw_1.Object.pafr_calibr[il_fila]	=	ias_campo[8]
	dw_1.Object.cocc_codigo[il_fila]	=	Integer(ias_campo[9])
	dw_1.Object.pafr_ccajas[il_fila]	=	Long(ias_campo[10])
	dw_1.Object.vari_codigo[il_fila] =  Integer(ias_campo[11])
	dw_1.Object.frio_tipofr[il_fila] 	=  ias_campo[12]
	dw_1.Object.vari_codrot[il_fila] =  Integer(ias_campo[13])
End If

end event

event ue_antesguardar;call super::ue_antesguardar;Integer	li_cont
String	ls_mensaje, ls_colu[]

If Isnull(dw_1.Object.plde_origen[il_fila]) OR dw_1.Object.plde_origen[il_fila] = 0 Then
	li_cont ++
	ls_mensaje 			= ls_mensaje + "~nCódigo de Planta Origen"
	ls_colu[li_cont]	= "plde_origen"
End If

//If Isnull(dw_1.Object.pefr_codigo[il_fila]) OR dw_1.Object.pefr_codigo[il_fila] = 0 Then
//	li_cont ++
//	ls_mensaje 			= ls_mensaje + "~nCódigo de Periodo Frio"
//	ls_colu[li_cont]	= "pefr_codigo"
//End If

If Isnull(dw_1.Object.prod_codigo[il_fila]) OR dw_1.Object.prod_codigo[il_fila] = 0 Then
	li_cont ++
	ls_mensaje 			= ls_mensaje + "~nCódigo de Productor"
	ls_colu[li_cont]	= "prod_codigo"
End If

If Isnull(dw_1.Object.prod_codrot[il_fila]) OR dw_1.Object.prod_codrot[il_fila] = 0 Then
	li_cont ++
	ls_mensaje 			= ls_mensaje + "~nCódigo de Productor Rotulado"
	ls_colu[li_cont]	= "prod_codrot"
End If

If Isnull(dw_1.Object.vari_codigo[il_fila]) OR dw_1.Object.vari_codigo[il_fila] = 0 Then
	li_cont ++
	ls_mensaje 			= ls_mensaje + "~nCódigo de Variedad"
	ls_colu[li_cont]	= "vari_codigo"
End If

If Isnull(dw_1.Object.vari_codrot[il_fila]) OR dw_1.Object.vari_codrot[il_fila] = 0 Then
	li_cont ++
	ls_mensaje 			= ls_mensaje + "~nCódigo de Variedad Rotulada"
	ls_colu[li_cont]	= "vari_codigo"
End If

If Isnull(dw_1.Object.emba_codigo[il_fila]) OR dw_1.Object.emba_codigo[il_fila] ="" Then
	li_cont ++
	ls_mensaje			= ls_mensaje + "~nCódigo de Embalaje"
	ls_colu[li_cont]	= "emba_codigo"
End If

If Isnull(dw_1.Object.frio_tipofr[il_fila]) OR dw_1.Object.frio_tipofr[il_fila] ="" Then
	li_cont ++
	ls_mensaje			= ls_mensaje + "~nTipo de Frío"
	ls_colu[li_cont]	= "frio_tipofr"
End If

If Isnull(dw_1.Object.pafr_calibr[il_fila]) OR dw_1.Object.pafr_calibr[il_fila] ="" Then
	li_cont ++
	ls_mensaje			= ls_mensaje + "~nCalibre del Pallet"
	ls_colu[li_cont]	= "pafr_calibr"
End If

//If Isnull(dw_1.Object.cocc_codigo[il_fila]) OR dw_1.Object.cocc_codigo[il_fila] = 0 Then
//	li_cont ++
//	ls_mensaje 			= ls_mensaje + "~nCondición Control de Calidad"
//	ls_colu[li_cont]	= "cocc_codigo"
//End If

If Isnull(dw_1.Object.pafr_ccajas[il_fila]) OR dw_1.Object.pafr_ccajas[il_fila] = 0 Then
	li_cont ++
	ls_mensaje 			= ls_mensaje + "~nCantidad de Cajas"
	ls_colu[li_cont]	= "pafr_ccajas"
End If

If Isnull(dw_1.Object.pafr_fecemb[il_fila]) OR dw_1.Object.pafr_fecemb[il_fila] = Date('01/01/1900') Then
	li_cont ++
	ls_mensaje 			= ls_mensaje + "~nFecha Embalaje"
	ls_colu[li_cont]	= "pafr_fecemb"
End If

If Isnull(dw_1.Object.prod_codigo[il_fila]) OR dw_1.Object.prod_codigo[il_fila] = 0 Then
	li_cont ++
	ls_mensaje 			= ls_mensaje + "~nProductor Real"
	ls_colu[li_cont]	= "prod_codigo"
End If

If Isnull(dw_1.Object.prod_codrot[il_fila]) OR dw_1.Object.prod_codrot[il_fila] = 0 Then
	li_cont ++
	ls_mensaje 			= ls_mensaje + "~nProductor Rotulado"
	ls_colu[li_cont]	= "prod_codrot"
End If

If Isnull(dw_1.Object.prbr_codpre[il_fila]) OR dw_1.Object.prbr_codpre[il_fila] = 0 Then
	li_cont ++
	ls_mensaje 			= ls_mensaje + "~nPredio Real"
	ls_colu[li_cont]	= "prbr_codpre"
End If

If Isnull(dw_1.Object.pafr_huert1[il_fila]) OR dw_1.Object.pafr_huert1[il_fila] = 0 Then
	li_cont ++
	ls_mensaje 			= ls_mensaje + "~nPredio Rotulado"
	ls_colu[li_cont]	= "pafr_huert1"
End If

If Isnull(dw_1.Object.prcc_codigo[il_fila]) OR dw_1.Object.prcc_codigo[il_fila] = 0 Then
	li_cont ++
	ls_mensaje 			= ls_mensaje + "~nCuartel Real"
	ls_colu[li_cont]	= "prcc_codigo"
End If

If Isnull(dw_1.Object.pafr_cuart1[il_fila]) OR dw_1.Object.pafr_cuart1[il_fila] = 0 Then
	li_cont ++
	ls_mensaje 			= ls_mensaje + "~nCuartel Rotulado"
	ls_colu[li_cont]	= "pafr_cuart1"
End If

If li_cont > 0 Then
	MessageBox("Error de Consistencia", "Falta el ingreso de : " + ls_mensaje + ".", StopSign!, Ok!)
	dw_1.SetColumn(ls_colu[1])
	dw_1.SetFocus()
	Message.DoubleParm = -1
Else
	istr_mant.Argumento[11]=String(Long(istr_mant.Argumento[11])-Long(dw_1.Object.pafr_ccajas[il_fila]))
End If

end event

event ue_nuevo;ib_ok = True

This.TriggerEvent("ue_guardar")
If Message.DoubleParm = -1 Then ib_ok = False

If ib_ok = False Then Return

wf_nuevo()

dw_1.SetFocus()

If Long(istr_mant.argumento[11])<1 Then
	MessageBox("Atención", "Se ha completado las cajas del Pallet.")
	pb_salir.TriggerEvent(Clicked!)
Else
	
	If Integer(istr_Mant.Argumento[21]) > 0 Then
		dw_1.Object.plde_origen[il_Fila]	=	Integer(istr_Mant.Argumento[21])
	Else
		dw_1.Object.plde_origen[il_Fila]	=	Integer(istr_mant.argumento[6])
	End If

	dw_1.Object.clie_codigo[il_Fila]	=	Integer(istr_mant.argumento[1])
	dw_1.Object.paen_numero[il_Fila]	=	Long(istr_mant.argumento[2])
	dw_1.Object.espe_codigo[il_Fila]	=	Integer(istr_mant.argumento[3])
	dw_1.Object.vari_codigo[il_Fila]	=	Integer(istr_mant.argumento[4])
	
	iuo_variedad.existe(dw_1.Object.espe_codigo[il_fila],Integer(istr_mant.argumento[4]),TRUE,SQLCA)
	
	dw_1.Object.vari_codrot[il_Fila]	=	iuo_variedad.varirelaci
	dw_1.Object.pafr_varrot[il_Fila] 	=	iuo_variedad.varirelaci
	dw_1.Object.vari_nombre[il_Fila]	=	istr_mant.argumento[5]
	dw_1.Object.frio_tipofr[il_Fila]		=	istr_mant.argumento[10]
	dw_1.Object.etiq_codigo[il_Fila]	=	Integer(istr_mant.argumento[9])
	dw_1.Object.plde_codigo[il_Fila]	=	Integer(istr_mant.argumento[6])
	dw_1.Object.pafr_estemb[il_Fila]	=	1
	dw_1.Object.cate_codigo[il_Fila]	=	Integer(istr_mant.argumento[13])
	dw_1.Object.emba_codigo[il_Fila]	=	istr_mant.argumento[14]
	dw_1.Object.pefr_codigo[il_Fila]	=	1
	
	dw_1.Object.pafr_ccajas[il_Fila]	=	Integer(istr_mant.argumento[11])
	dw_calibres.Retrieve(Integer(istr_Mant.Argumento[3]), Integer(istr_Mant.Argumento[7]), Integer(istr_Mant.Argumento[8]))
	
	If gstr_parempresa.Productor > 0 Then
		dw_1.Object.prod_codigo[il_Fila]	=	gstr_parempresa.Productor
		dw_1.Object.prod_codrot[il_Fila]	=	gstr_parempresa.Productor
		
		idwc_Predio.Retrieve(gstr_parempresa.Productor)
		idwc_Prediorot.Retrieve(gstr_parempresa.Productor) 
	End If

//	If istr_Mant.Argumento[20]	= '2' Then
//		dw_1.Object.prbr_codpre.Protect				=	1
//		dw_1.Object.prbr_codpre.BackGround.Color	=	RGB(192,192,192)
//		dw_1.Object.prcc_codigo.Protect				=	1
//		dw_1.Object.prcc_codigo.BackGround.Color	=	RGB(192,192,192)
//	End If
	
//	If istr_Mant.Argumento[20]	= '3' OR istr_Mant.Argumento[20]	= '2' OR istr_Mant.Argumento[20]	= '10' Then
//		dw_1.Object.lote_codigo.Protect				=	1
//		dw_1.Object.lote_codigo.BackGround.Color	=	RGB(192,192,192)
//		dw_1.Object.pafr_tipdoc.Protect				=	1
//		dw_1.Object.pafr_tipdoc.BackGround.Color	= RGB(192,192,192)
//		If istr_Mant.Argumento[20]	<> '3' Then
//			dw_1.Object.pafr_docrel.Protect				=	1
//			dw_1.Object.pafr_docrel.BackGround.Color	= RGB(192,192,192)
//			dw_1.Object.pafr_docrel[il_fila]				= Long(istr_mant.argumento[23])
//		End If
//		dw_1.Object.pafr_tipdoc[il_fila]				= 2
//	End If
//	
//	If istr_Mant.Argumento[20]	= '8' Then
//		dw_1.Object.lote_codigo.Protect				=	1
//		dw_1.Object.lote_codigo.BackGround.Color	=	RGB(192,192,192)
//		dw_1.Object.pafr_tipdoc.Protect				=	1
//		dw_1.Object.pafr_tipdoc.BackGround.Color	= RGB(192,192,192)
//		dw_1.Object.pafr_docrel.Protect				=	1
//		dw_1.Object.pafr_docrel.BackGround.Color	= RGB(192,192,192)	
//	End If
//	
//	If istr_Mant.Argumento[20]	= '5' Then
//		dw_1.Object.pafr_tipdoc.Protect				=	1
//		dw_1.Object.pafr_tipdoc.BackGround.Color	=	RGB(192,192,192)
//		dw_1.Object.pafr_docrel.Protect				=	1
//		dw_1.Object.pafr_docrel.BackGround.Color	=	RGB(192,192,192)
//	End If
	
End If
end event

event open;x	= 100
y	= 450

PostEvent("ue_recuperadatos")

istr_mant = Message.PowerObjectParm

If istr_mant.solo_consulta Then dw_1.Enabled	=	False

iuo_Lote								=	Create uo_lotesfrutagranel
iuo_Predio							=	Create uo_ProdPredio
iuo_Cuartel							=	Create uo_ProdCuarteles
iuo_variedad						=  Create uo_variedades
iuo_FechaMovto					=	Create uo_FechaMovto
iuo_pfrio 							=	Create uo_periodofrio

dw_1.GetChild("clie_codigo", dw_cliente)
dw_1.GetChild("espe_codigo", dw_especies)
dw_1.GetChild("vari_codigo", dw_variedades)
dw_1.GetChild("vari_codrot", dw_varirot)
dw_1.GetChild("frio_tipofr", dw_tipofrio)
dw_1.GetChild("cate_codigo", dw_categoria)
dw_1.GetChild("etiq_codigo", dw_etiquetas)
dw_1.GetChild("plde_codigo", dw_plantadesp)
dw_1.GetChild("plde_origen", dw_plantaorigen)
dw_1.GetChild("pefr_codigo", dw_periodofrio)
dw_1.GetChild("prod_codigo", dw_productor)
dw_1.GetChild("prod_codrot", dw_prodrot)
dw_1.GetChild("emba_codigo", dw_embalajes)
dw_1.GetChild("pafr_calibr", dw_calibres)
dw_1.GetChild("pafr_calrot", dw_calibrot)
dw_1.GetChild("cocc_codigo", dw_condicion)
dw_1.GetChild("prbr_codpre", idwc_Predio)
dw_1.GetChild("prcc_codigo", idwc_Cuartel)
dw_1.GetChild("pafr_huert1", idwc_Prediorot)
dw_1.GetChild("pafr_cuart1", idwc_Cuarterot)

dw_cliente.SetTransObject(sqlca)
dw_especies.SetTransObject(sqlca)
dw_variedades.SetTransObject(sqlca)
dw_varirot.SetTransObject(sqlca)
dw_tipofrio.SetTransObject(sqlca)
dw_categoria.SetTransObject(sqlca)
dw_etiquetas.SetTransObject(sqlca)
dw_plantadesp.SetTransObject(sqlca)
dw_periodofrio.SetTransObject(sqlca)
dw_plantaorigen.SetTransObject(sqlca)
dw_productor.SetTransObject(sqlca)
dw_prodrot.SetTransObject(sqlca)
dw_embalajes.SetTransObject(sqlca)
dw_calibres.SetTransObject(sqlca)
dw_calibrot.SetTransObject(sqlca)
dw_condicion.SetTransObject(sqlca)
idwc_Predio.SetTransObject(sqlca)
idwc_Cuartel.SetTransObject(sqlca)
idwc_Prediorot.SetTransObject(sqlca)
idwc_Cuarterot.SetTransObject(sqlca)

dw_cliente.Retrieve()
dw_especies.Retrieve(Integer(istr_Mant.Argumento[1]))
dw_variedades.Retrieve(Integer(istr_Mant.Argumento[3]), Integer(istr_Mant.Argumento[1]))
dw_varirot.Retrieve(Integer(istr_Mant.Argumento[3]), Integer(istr_Mant.Argumento[1]))
dw_tipofrio.Retrieve()
dw_categoria.Retrieve()
dw_etiquetas.Retrieve(Integer(istr_Mant.Argumento[1]))
dw_plantadesp.Retrieve(Integer(istr_Mant.Argumento[1]))
dw_plantaorigen.Retrieve(Integer(istr_Mant.Argumento[1]))
dw_periodofrio.Retrieve()
dw_productor.Retrieve(-1)
dw_prodrot.Retrieve(-1)
dw_condicion.Retrieve()

idwc_Predio.Retrieve(Integer(istr_Mant.Argumento[25]))
idwc_Cuartel.Retrieve(Integer(istr_Mant.Argumento[25]),Integer(istr_Mant.Argumento[27]))
idwc_Prediorot.Retrieve(Integer(istr_Mant.Argumento[26]))
idwc_Cuarterot.Retrieve(Integer(istr_Mant.Argumento[26]),Integer(istr_Mant.Argumento[28]))

If dw_embalajes.Retrieve(Integer(istr_Mant.Argumento[7]), &
							 Integer(istr_Mant.Argumento[8]),&
							 Integer(istr_Mant.Argumento[1])) = 0 Then
	dw_embalajes.InsertRow(0)
Else
	If gstr_paramplanta.etiquetaembalaje = 1 Then
		dw_embalajes.SetFilter("etiq_codigo = " + istr_mant.argumento[9])
		dw_embalajes.Filter()							 
	End If
End If

If dw_calibres.Retrieve(Integer(istr_Mant.Argumento[3]), Integer(istr_Mant.Argumento[7]), &
							   Integer(istr_Mant.Argumento[8]),Integer(istr_Mant.Argumento[1])) = 0 Then
	dw_calibres.InsertRow(0)
Else
	dw_calibres.SetFilter("enva_tipoen = " + istr_mant.argumento[7] + " AND " + &
	                      "enva_codigo = " + istr_mant.argumento[8] )
	dw_calibres.Filter()							 
End If

If dw_calibrot.Retrieve(Integer(istr_Mant.Argumento[3]), &
							   Integer(istr_Mant.Argumento[7]), &
							   Integer(istr_Mant.Argumento[8]),&
								Integer(istr_Mant.Argumento[1])) = 0 Then
	dw_calibrot.InsertRow(0)
Else
	dw_calibrot.SetFilter("enva_tipoen = " + istr_mant.argumento[7] + " AND " + &
	                      "enva_codigo = " + istr_mant.argumento[8] )
	dw_calibrot.Filter()							 
End If

idwc_Predio.InsertRow(0)
idwc_Cuartel.InsertRow(0)
idwc_Prediorot.InsertRow(0)
idwc_Cuarterot.InsertRow(0)

dw_1.SetTransObject(sqlca)
istr_mant.dw.ShareData(dw_1)

//istr_mant.argumento[11] = String(Integer(istr_mant.argumento[11]) - Integer(istr_mant.argumento[90]))

end event

type pb_ultimo from w_mant_detalle_csd`pb_ultimo within w_mant_deta_palletfruta
end type

type pb_siguiente from w_mant_detalle_csd`pb_siguiente within w_mant_deta_palletfruta
end type

type pb_anterior from w_mant_detalle_csd`pb_anterior within w_mant_deta_palletfruta
end type

type pb_primero from w_mant_detalle_csd`pb_primero within w_mant_deta_palletfruta
end type

type pb_cancela from w_mant_detalle_csd`pb_cancela within w_mant_deta_palletfruta
integer x = 3035
integer y = 328
end type

type pb_acepta from w_mant_detalle_csd`pb_acepta within w_mant_deta_palletfruta
integer x = 3026
integer y = 116
end type

type pb_salir from w_mant_detalle_csd`pb_salir within w_mant_deta_palletfruta
integer x = 3035
integer y = 548
end type

type dw_1 from w_mant_detalle_csd`dw_1 within w_mant_deta_palletfruta
integer x = 96
integer width = 2807
integer height = 1476
string dataobject = "dw_mant_palletfruta"
end type

event dw_1::itemchanged;call super::itemchanged;String	ls_columna, ls_Nula
Integer	li_Planta, li_Especie, li_Predio, li_find, li_cuartpro, li_Prediorot
DateTime ldt_fecha
Long		ll_Productor, ll_Productorrot
Date     ld_null

SetNull(ls_Nula)
SetNull(ld_null)

ls_columna	=	dwo.name

Choose Case ls_columna
	Case "prod_codigo"
		If NoExisteProductor(data) Then
			dw_1.SetItem(row, ls_columna, Integer(ls_Nula))
			Return 1
		ElseIf idwc_Predio.Retrieve(Long(Data)) = 0 Then
			MessageBox("Atención", "Productor no tiene definido Predios")
		Else
			This.Object.pafr_ggncod[Row] = f_AsignaGGN(Long(Data), This.Object.prbr_codpre[Row], This.Object.espe_codigo[Row], This.Object.pafr_fecemb[Row])
			This.Object.prod_codrot[row]	=	Long(Data)
			
			idwc_Cuartel.Reset()
			idwc_Cuartel.InsertRow(0)
		End If
		
		If idwc_Prediorot.Retrieve(Long(Data)) = 0 Then
			
		Else
			This.Object.prod_codrot[row]	=	Long(Data)	
			idwc_Cuarterot.Reset()
			idwc_Cuarterot.InsertRow(0)
		End If
		
	Case "prod_codrot"
		If NoExisteProductor(data) Then
			dw_1.SetItem(row, ls_columna, Integer(ls_Nula))
			Return 1
		ElseIf idwc_Prediorot.Retrieve(Long(Data)) = 0 Then
			MessageBox("Atención", "Productor no tiene definido Predios Rotulados")
		Else
			This.Object.pafr_ggncod[Row] = f_AsignaGGN(Long(Data), This.Object.pafr_huert1[Row], This.Object.espe_codigo[Row], This.Object.pafr_fecemb[Row])
			idwc_Cuarterot.Reset()
			idwc_Cuarterot.InsertRow(0)
		End If
		
		dw_1.SetItem(row, "pafr_huert1", Integer(ls_Nula))
		dw_1.SetItem(row, "pafr_cuart1", Integer(ls_Nula))

	Case "lote_codigo"
		li_Planta	=	This.Object.plde_origen[row]
		li_Especie	=	This.Object.espe_codigo[row]
		
		If not iuo_Lote.Existe(li_Planta, li_Especie, Integer(Data), True, Sqlca) Then
			dw_1.SetItem(row, ls_columna, Integer(ls_Nula))
			Return 1
		Else
			This.Object.prod_codigo[row]	=	iuo_Lote.Productor
			This.Object.prod_codrot[row]	=	iuo_Lote.Productor
		End If

	Case "prbr_codpre"
		ll_Productor	=	This.Object.prod_codigo[row]
		
		If not iuo_Predio.Existe(Integer(Data), ll_Productor, True, Sqlca) Then
			dw_1.SetItem(row, ls_columna, Integer(ls_Nula))
			Return 1
		ElseIf idwc_Cuartel.Retrieve(This.Object.prod_codigo[row], Integer(Data), Integer(istr_Mant.Argumento[3])) = 0 Then
			MessageBox("Atención", "Predio no tiene definido Cuarteles.~r~r" + &
							"Ingrese o Seleccione otro Predio o Productor.")
			dw_1.SetItem(row, ls_columna, Integer(ls_Nula))
			Return 1
		End If
		
		If idwc_Cuarterot.Retrieve(This.Object.prod_codigo[row], Integer(Data), Integer(istr_Mant.Argumento[3])) = 0 Then
			dw_1.SetItem(row, ls_columna, Integer(ls_Nula))
			Return 1
		End If
		
		This.Object.pafr_ggncod[Row] = f_AsignaGGN(This.Object.prod_codigo[Row], Long(Data), This.Object.espe_codigo[Row], This.Object.pafr_fecemb[Row])
		This.Object.pafr_huert1[row] = Long(data)
		
	Case "pafr_huert1"
		ll_Productorrot	=	This.Object.prod_codrot[row]
		
		If not iuo_Predio.Existe(Integer(Data), ll_Productorrot, True, Sqlca) Then
			dw_1.SetItem(row, ls_columna, Integer(ls_Nula))
			Return 1
		ElseIf idwc_Cuarterot.Retrieve(This.Object.prod_codrot[row], Integer(Data), Integer(istr_Mant.Argumento[3])) = 0 Then
			MessageBox("Atención", "Predio no tiene definido Cuarteles.~r~r" + &
							"Ingrese o Seleccione otro Predio o Productor.")
			dw_1.SetItem(row, ls_columna, Integer(ls_Nula))
			Return 1
		Else
			This.Object.pafr_ggncod[Row] = f_AsignaGGN(This.Object.prod_codrot[Row], Long(Data), This.Object.espe_codigo[Row], This.Object.pafr_fecemb[Row])
		End If	

	Case "prcc_codigo"
		ll_Productor	=	This.Object.prod_codigo[row]
		li_Predio		=	This.Object.prbr_codpre[row]
		
		If not iuo_Cuartel.Existe(ll_Productor, li_Predio,Integer(istr_Mant.Argumento[3]), Integer(Data), True, Sqlca,Integer(istr_Mant.Argumento[1])) Then
			dw_1.SetItem(row, ls_columna, Integer(ls_Nula))
			Return 1
		Else
			iuo_variedad.existe(dw_1.Object.espe_codigo[il_fila],iuo_Cuartel.Variedad,TRUE,SQLCA)
			
			This.Object.vari_codigo[row]	=	iuo_Cuartel.Variedad
			This.Object.vari_codrot[row]	=	iuo_variedad.varirelaci
		End If
		
		This.Object.pafr_cuart1[row] = Long(data)
		
	Case "pafr_cuart1"
		ll_Productorrot	=	This.Object.prod_codrot[row]
		li_Prediorot		=	This.Object.pafr_huert1[row]
		
		If not iuo_Cuartel.Existe(ll_Productorrot, li_Prediorot, Integer(istr_Mant.Argumento[3]), Integer(Data), True, Sqlca, Integer(istr_Mant.Argumento[1])) Then
			dw_1.SetItem(row, ls_columna, Integer(ls_Nula))
			Return 1
		Else
			iuo_variedad.existe(dw_1.Object.espe_codigo[il_fila],iuo_Cuartel.Variedad,TRUE,SQLCA)
			
			This.Object.vari_codigo[row]	=	iuo_Cuartel.Variedad
			This.Object.vari_codrot[row]	=	iuo_variedad.varirelaci
		End If	

	Case "pafr_fecemb"
		ldt_Fecha	=	DateTime(Date(Mid(data,1,10)))
		
		If NOT iuo_FechaMovto.Valida_FechaMovto(Date(ldt_Fecha)) Then
			This.SetItem(row,"pafr_fecemb",ld_Null)
			This.SetFocus()
			Return 1
		End If
		
		FueradeNorma(ls_Columna, Mid(data,1,10))

	Case "pafr_calibr"
		If NoExisteCalibre(data, ls_columna) OR Duplicado(Upper(data)) Then
			dw_1.SetItem(row, ls_columna, ls_Nula)
			Return 1
		Else
			//Chequeo Fuera de Norma
			FueradeNorma(ls_Columna, Data)
			This.Object.pafr_calrot[Row]	=	Data
		End If

	Case "pafr_calrot"
		If NoExisteCalibre(data, ls_columna) Then
			dw_1.SetItem(row, ls_columna, ls_Nula)
			Return 1
		End If
		
	Case "emba_codigo"
		
		If NOT buscanombreembalaje(data) Then
			dw_1.SetItem(row, ls_columna, ls_Nula)
			Return 1
		Else
			FueradeNorma(ls_Columna, Data)
		End If	

	Case "pafr_ccajas"
			If Long(data) < 0 Then
				dw_1.SetItem(row, ls_columna, Long(istr_mant.Argumento[11]))
				Return 1
			End If
			
			If Long(data) > Long(istr_mant.Argumento[11]) Then
				MessageBox("Atención", "Cajas ingresadas sobrepasan las " + istr_mant.Argumento[11] + &
								" cajas del Pallet")
				dw_1.SetItem(row, ls_columna, Long(istr_mant.Argumento[11]))
				Return 1
			End If
	
	Case "vari_codigo"
		If NOT iuo_variedad.existe(dw_1.Object.espe_codigo[il_fila],integer(data),TRUE,SQLCA) Then
			dw_1.SetItem(row, ls_columna, Integer(istr_mant.Argumento[4]))
			Return 1
		Else
			li_find	=	idwc_Cuartel.Find("espe_codigo = " + String(dw_1.Object.espe_codigo[il_fila]) + " and " + &
													"vari_codigo = " + data, 1, idwc_Cuartel.RowCount())
			If li_find < 0 Then li_find = 0
			
			Choose Case li_find
				Case 0
					MessageBox("Advertencia", "La variedad ingresada no pertenece a ningun cuartel de este predio.")
				
				Case Else
					li_cuartpro	=	idwc_Cuartel.GetItemNumber(li_find, "prcc_codigo")
					If li_cuartpro <> This.Object.prcc_codigo[row] Then
						MessageBox("Advertencia", "La variedad ingresada no pertenece al cuartel seleccionado.~r~n" + &
														  "El cuartel al que corresponde dicha variedad es el " + String(li_cuartpro))
					End If
				
			End Choose
			dw_1.Object.vari_codrot[Row] = iuo_variedad.varirelaci
     		dw_1.Object.pafr_varrot[Row] = iuo_variedad.varirelaci
		End If	
	
	Case "vari_codrot"
		If NOT iuo_variedad.existe(dw_1.Object.espe_codigo[il_fila],integer(data),TRUE,SQLCA) Then
			dw_1.SetItem(row, ls_columna, Integer(istr_mant.Argumento[4]))
			Return 1
		Else
			dw_1.Object.pafr_varrot[Row] = integer(data)
		End If
		
	Case "pefr_codigo"
		If NOT iuo_pfrio.ofp_recupera_periodofrio(sqlca, Integer(data), True) Then
			dw_1.Object.pefr_codigo[Row] = Integer(ls_Nula)
	
			Return 1
		End If
			
		
End Choose
end event

event dw_1::clicked;call super::clicked;String	ls_columna

ls_columna = dwo.name

Choose Case ls_columna
	Case "buscaprod"
		buscaprod()
	
End Choose
End event

event dw_1::itemerror;call super::itemerror;Return 1
End event

