$PBExportHeader$w_mant_deta_generapalletdesdecajas_grane.srw
$PBExportComments$Mantención Detalle de Despacho Pallets
forward
global type w_mant_deta_generapalletdesdecajas_grane from w_mant_detalle_csd
end type
type dw_2 from datawindow within w_mant_deta_generapalletdesdecajas_grane
end type
end forward

global type w_mant_deta_generapalletdesdecajas_grane from w_mant_detalle_csd
integer width = 2688
integer height = 1768
dw_2 dw_2
end type
global w_mant_deta_generapalletdesdecajas_grane w_mant_deta_generapalletdesdecajas_grane

type variables
DataWindowChild   dw_calibres		,	dw_productor 	,	&
                  dw_embalajes	,	dw_variedades 	,	&
                  dw_plantadesp	,	dw_especies		,	&
                  dw_etiquetas	,	dw_condiciones	,	dw_cliente,&
						idwc_predio		, 	idwc_cuartel
						
Integer il_lote						


end variables

forward prototypes
public function boolean existevariecab (integer as_valor)
public function boolean noexistecalibre (string as_valor)
public function boolean noexisteproductor (string ls_columna)
public subroutine buscaprod ()
public function boolean varificaproductor (long ll_productor)
public function boolean noexistepredio (long ai_productor, integer ai_predio)
public function boolean noexistecaja (long al_numero)
public subroutine limpiaingreso ()
public function boolean duplicado (long al_numero)
end prototypes

public function boolean existevariecab (integer as_valor);Integer	li_especie, li_variedad, li_cliente
String	ls_nombre, ls_calibre
			//ls_secacod

li_cliente					= integer(istr_mant.argumento[1])
li_especie					= integer(istr_mant.argumento[3])
li_variedad					= as_valor

//ls_secacod					= istr_mant.argumento[12]

SELECT	vaca_calibr    INTO : ls_calibre
	FROM	dba.variecalibre
	WHERE	espe_codigo		= :li_especie  and &
			vari_codigo    = :li_variedad ;//and &
	//		seca_codigo		= : ls_secacod ;
			
IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla Variecalibre")
	RETURN False
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención","Código de Variedad-Calibre no Existe.", Exclamation!, Ok!)
	RETURN False
END IF

RETURN TRUE
end function

public function boolean noexistecalibre (string as_valor);String	ls_codigo, ls_calibr
Integer	li_cliente, li_especie, li_variedad
Long	   registros

dw_1.accepttext()

li_cliente	= Integer(istr_mant.argumento[1])
li_especie	= Integer(istr_mant.argumento[3])
li_variedad	= Integer(istr_mant.argumento[4])
ls_codigo   = Upper(as_valor)


SELECT	vaca_calibr
	INTO	:ls_calibr
	FROM  dba.variecalibre
	WHERE	espe_codigo =	:li_especie
	AND	vari_codigo =	:li_variedad 
	AND	vaca_calibr	=	:ls_codigo ;
		
IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla VarieCalibre")
	RETURN True
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención", "Calidad no Asignada para esta Variedad, Ingrese otra.", &
					Exclamation!, OK!)
	RETURN True
ELSE
	dw_1.SetItem(il_fila, "pafr_calibr", ls_codigo)
	RETURN False
END IF

end function

public function boolean noexisteproductor (string ls_columna);String	ls_nombre
Integer	li_cliente
Boolean	lb_retorna = True
Long		ll_codigo

li_cliente	 = Integer(istr_mant.argumento[1])
ll_codigo 	 = Long(ls_columna)

IF istr_mant.argumento[17] = '1'and istr_mant.argumento[24] <> '1' THEN
	IF Pos(istr_mant.argumento[15], "," + String(ll_codigo)) = 0 THEN
		messagebox("Atención","Productor no corresponde a Pallets originales", Exclamation!, Ok!)
		RETURN lb_Retorna
	END IF
END IF

	SELECT	prod_nombre INTO :ls_nombre
	FROM    dba.PRODUCTORES
	WHERE	   prod_codigo = :ll_codigo ;
	
	IF sqlca.SQLCode = -1 THEN
		F_errorbasedatos(sqlca,"Lectura tabla Productores")
		Return lb_Retorna
		
	ELSEIF sqlca.SQLCode = 100 THEN
		MessageBox("Atención", "Código de Productor no ha sido creado, Ingrese otro Código.", &
		Exclamation!, OK!)
		Return lb_Retorna
	ELSE
		istr_mant.argumento[13]	=	string(ll_codigo)
		dw_1.SetItem (il_fila, "prod_codigo", ll_codigo)
		dw_1.SetItem (il_fila, "productores_prod_nombre", ls_nombre)
		lb_retorna = False
		RETURN lb_retorna
	END IF

Return lb_retorna
end function

public subroutine buscaprod ();dw_1.Modify("buscaprod.border = 0")
dw_1.Modify("buscaprod.border = 5")
istr_busq.Argum[1] = istr_mant.Argumento[1]

OpenWithParm(w_busc_productores, istr_busq)

istr_busq	= Message.PowerObjectParm

IF istr_busq.Argum[4] = "" THEN
	dw_1.SetColumn("prod_codigo")
	dw_1.SetFocus()
ELSE
	dw_1.SetItem(il_fila, "prod_codigo", Long(istr_busq.Argum[4]))
	dw_1.SetItem(il_fila, "productores_prod_nombre", istr_busq.Argum[5])
	IF istr_mant.argumento[14] = '1' THEN
		NoExisteProductor(istr_busq.Argum[4])
	END IF
	
	dw_1.SetColumn("pafr_calibr")
	dw_1.SetFocus()
END IF

dw_1.Modify("buscaprod.border = 0")
dw_1.Modify("buscaprod.border = 6")

RETURN
end subroutine

public function boolean varificaproductor (long ll_productor); Integer li_cont=0, li_cliente,li_planta
 
 li_cliente=Integer(istr_mant.argumento[1])
 li_planta=Integer(istr_mant.argumento[21])

 SELECT count (prod_codigo) 
 INTO :li_cont  
 FROM dba.prodpacking  
 WHERE prod_codigo = :ll_productor AND  
       plde_codigo = :li_planta;
			
IF (sqlca.sqlcode)= -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla Prodpacking")
	RETURN FALSE
ELSEIF li_cont>0 THEN
	RETURN TRUE
ELSE
	MessageBox("Atención", "Código de Productor No Asignado a Packing Origen.", &
					Exclamation!, OK!)
	RETURN FALSE
END IF


end function

public function boolean noexistepredio (long ai_productor, integer ai_predio);Integer li_existe

SELECT Count(prod_codigo) 
INTO :li_existe
FROM dba.spro_prodpredio
WHERE prod_codigo = :ai_productor
AND	prpr_codigo = :ai_predio;

IF (sqlca.sqlcode)= -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla spro_prediosproductor")
	RETURN FALSE
ELSEIF li_existe>0 THEN
	RETURN TRUE
ELSE
	MessageBox("Atención", "Codigo de Predio no esta asignado a Productor.", &
					Exclamation!, OK!)
	RETURN FALSE
END IF
end function

public function boolean noexistecaja (long al_numero);Integer	li_Filaes,li_Cliente,li_Planta,li_especie,li_variedad, &
         li_predio,li_huerto,li_cuarte,li_etique,li_embdor,li_selecc, &
			li_pesado,li_estado,li_varrot,li_catego,li_cespak,li_nrlote,li_Respuesta, &
			li_enc_estado, li_enc_PCoPDA
Long		ll_numpal,ll_docrel,ll_productor,ll_numgia,ll_numcaja
Date		ld_fecha,ld_fecemb
String	ls_embala,ls_calibr,ls_cean14,ls_regcap, ls_productor

li_Cliente	=	Integer(istr_mant.argumento[1])
li_Planta	=	Integer(istr_mant.argumento[6])
ll_numcaja	=	al_numero

SELECT espe_codigo,vari_codigo,emba_codigo,etiq_codigo,
		capr_docrel,capr_fecemb,prod_codigo,
		prod_predio,prod_huerto,prod_cuarte,
		capr_fecemb,capr_calibr,capr_embala,capr_selecc,capr_pesado,
		capr_cean14,capr_numpal,capr_regcap,capr_estado,capr_varrot,
		capr_numgia,cate_codigo,capr_cespak,capr_nrlote
		INTO	:li_especie, :li_variedad, :ls_embala, :li_etique,
		:ll_docrel,:ld_fecha,:ll_productor,
		:li_predio,:li_huerto,:li_cuarte,
		:ld_fecemb,:ls_calibr,:li_embdor,:li_selecc,:li_pesado,
		:ls_cean14,:ll_numpal,:ls_regcap,:li_estado,:li_varrot,
		:ll_numgia,:li_catego,:li_cespak,:li_nrlote
		FROM dba.spro_cajasprod
		WHERE clie_codigo 	= 	:li_Cliente
		AND   plde_codigo		= 	:li_Planta
		AND   capr_numero 	= 	:ll_numcaja;

IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura tabla spro_cajasprod")
	RETURN True
ELSEIF sqlca.SQLCode = 100 THEN
		MessageBox("Cuidado","Número de Caja No Existe en Tabla Respectiva!! ", &
		StopSign!) 
      RETURN True
END IF

IF Not IsNull(ll_numpal) AND ll_numpal > 0 THEN
	MessageBox("Cuidado","Número de Caja Ya está Asignado a un Pallet !! ", &
	StopSign!) 
	RETURN True
END IF

// Para la primera vez.
IF Isnull(istr_mant.argumento[3]) OR istr_mant.argumento[3]="" THEN 
	istr_mant.argumento[3]	=	String(li_especie)	//	especie
END IF
IF Isnull(istr_mant.argumento[4]) OR istr_mant.argumento[4]="" THEN 
	istr_mant.argumento[4]	=	String(li_variedad)	// variedad
END IF
IF Isnull(istr_mant.argumento[7]) OR istr_mant.argumento[7]="" THEN
	istr_mant.argumento[7]	=	String(ls_embala)		//	embalaje
END IF
IF Isnull(istr_mant.argumento[9]) OR istr_mant.argumento[9]="" THEN 
	istr_mant.argumento[9]	=	String(li_etique)		//	li_etiqueta
END IF

SELECT prod_nombre 
	INTO :ls_productor 
	FROM dba.productores
	WHERE prod_codigo	=	:ll_productor;

dw_1.Object.clie_codigo[il_fila]	=	li_Cliente
dw_1.Object.plde_codigo[il_fila]	=	li_Planta
dw_1.Object.capr_numero[il_Fila]	=	al_numero
dw_1.Object.capr_docrel[il_fila]	=	ll_docrel
dw_1.Object.capr_fecemb[il_fila]	=	ld_fecha
dw_1.Object.prod_codigo[il_fila]	=	ll_productor
dw_1.Object.espe_codigo[il_fila]	=	li_especie
dw_1.Object.vari_codigo[il_fila]	=	li_variedad
dw_1.Object.prod_predio[il_fila]	=	li_predio
dw_1.Object.prod_huerto[il_fila]	=	li_huerto
dw_1.Object.prod_cuarte[il_fila]	=	li_cuarte
dw_1.Object.emba_codigo[il_fila]	=	ls_embala
dw_1.Object.etiq_codigo[il_fila]	=	li_etique
dw_1.Object.capr_fecemb[il_fila]	=	ld_fecemb
dw_1.Object.capr_calibr[il_fila]	=	ls_calibr
dw_1.Object.capr_embala[il_fila]	=	li_embdor
dw_1.Object.capr_selecc[il_fila]	=	li_selecc
dw_1.Object.capr_pesado[il_fila]	=	li_pesado
dw_1.Object.capr_cean14[il_fila]	=	ls_cean14
dw_1.Object.capr_numpal[il_fila]	=	ll_numpal
dw_1.Object.capr_regcap[il_fila]	=	ls_regcap
dw_1.Object.capr_estado[il_fila]	=	li_estado
dw_1.Object.capr_varrot[il_fila]	=	li_varrot
dw_1.Object.capr_numgia[il_fila]	=	ll_numgia
dw_1.Object.cate_codigo[il_fila]	=	li_catego
dw_1.Object.capr_cespak[il_fila]	=	li_cespak
dw_1.Object.capr_nrlote[il_fila]	=	li_nrlote

dw_1.Object.variedades_vari_nombre[il_fila]	=	ls_productor

IF dw_1.Rowcount() > 1 THEN
	IF (dw_1.Object.capr_fecemb[il_Fila]	<>	dw_1.Object.capr_fecemb[il_Fila - 1]) OR &
		(dw_1.Object.prod_codigo[il_Fila]	<>	dw_1.Object.prod_codigo[il_Fila - 1]) OR &
		(dw_1.Object.espe_codigo[il_Fila]	<>	dw_1.Object.espe_codigo[il_Fila - 1]) OR &
		(dw_1.Object.vari_codigo[il_Fila]	<>	dw_1.Object.vari_codigo[il_Fila - 1]) OR &
		(dw_1.Object.emba_codigo[il_Fila]	<> dw_1.Object.emba_codigo[il_Fila - 1]) OR &
		(dw_1.Object.etiq_codigo[il_Fila]	<>	dw_1.Object.etiq_codigo[il_Fila - 1]) OR &
		(dw_1.Object.capr_calibr[il_Fila]	<>	dw_1.Object.capr_calibr[il_Fila - 1]) OR &
		(dw_1.Object.cate_codigo[il_Fila]	<>	dw_1.Object.cate_codigo[il_Fila - 1]) OR &
	 	(dw_1.Object.capr_cespak[il_Fila]	<>	dw_1.Object.capr_cespak[il_Fila - 1]) THEN
		 
	   li_Respuesta = MessageBox("Error","Características Diferentes a Otras Cajas del Pallet !!, Desea Continuar ? ", &
										  Exclamation!, OKCancel!, 2)
		
		IF li_Respuesta = 1 THEN		
		 		RETURN False
		ELSE	
				LimpiaIngreso()
		  		RETURN True
		END IF		
   END IF
END IF

RETURN False

end function

public subroutine limpiaingreso ();Date		ld_Fecha
String	ls_Nula

SetNull(ld_Fecha)
SetNull(ls_Nula)

dw_1.Object.clie_codigo[il_Fila]	=	Integer(ls_Nula)
dw_1.Object.plde_codigo[il_Fila]	=	Integer(ls_Nula)
dw_1.Object.capr_numero[il_Fila]	=	Long(ls_Nula)
dw_1.Object.capr_docrel[il_Fila]	=	Long(ls_Nula)
dw_1.Object.capr_fecemb[il_Fila]	=	ld_Fecha
dw_1.Object.prod_codigo[il_Fila]	=	Long(ls_Nula)
dw_1.Object.espe_codigo[il_Fila]	=	Integer(ls_Nula)
dw_1.Object.vari_codigo[il_Fila]	=	Integer(ls_Nula)
dw_1.Object.prod_predio[il_Fila]	=	Integer(ls_Nula)
dw_1.Object.prod_huerto[il_Fila]	=	Integer(ls_Nula)
dw_1.Object.prod_cuarte[il_Fila]	=	Integer(ls_Nula)
dw_1.Object.emba_codigo[il_Fila]	=	ls_Nula
dw_1.Object.etiq_codigo[il_Fila]	=	Integer(ls_Nula)
dw_1.Object.capr_calibr[il_Fila]	=	ls_Nula
dw_1.Object.capr_embala[il_Fila]	=	Integer(ls_Nula)
dw_1.Object.capr_selecc[il_Fila]	=	Integer(ls_Nula)
dw_1.Object.capr_pesado[il_Fila]	=	Integer(ls_Nula)
dw_1.Object.capr_cean14[il_Fila]	=	ls_Nula
dw_1.Object.capr_numpal[il_Fila]	=	Long(ls_Nula)
dw_1.Object.capr_regcap[il_Fila]	=	ls_Nula
dw_1.Object.capr_estado[il_Fila]	=	Integer(ls_Nula)
dw_1.Object.capr_varrot[il_Fila]	=	Integer(ls_Nula)
dw_1.Object.capr_numgia[il_Fila]	=	Long(ls_Nula)
dw_1.Object.cate_codigo[il_Fila]	=	Integer(ls_Nula)
dw_1.Object.capr_cespak[il_Fila]	=	Integer(ls_Nula)
dw_1.Object.capr_nrlote[il_Fila]	=	Integer(ls_Nula)

end subroutine

public function boolean duplicado (long al_numero);Boolean	lb_Retorno
Long		ll_fila,ll_numcaja
String	ls_numero
Integer	li_Cliente, li_Planta

lb_Retorno	=	False
//ll_numcaja	=	Long(Trim(campo))
//ls_numero	=	campo
//String	ls_client, ls_nropal, ls_especi, ls_varied, ls_embala, ls_produc, ls_condic, ls_etique, &
//			ls_planta, ls_zonaco, ls_calibr, ls_huert1, ls_cuart1
//
//	
//ls_client = istr_mant.argumento[1]
//ls_nropal = istr_mant.argumento[2]
//ls_especi = istr_mant.argumento[3]
//ls_varied = istr_mant.argumento[4]
//ls_embala = istr_mant.argumento[7]
//ls_produc = String(dw_1.GetItemNumber(il_fila, "prod_codigo"))
//ls_condic = istr_mant.argumento[10]
//ls_etique = istr_mant.argumento[9]
//ls_planta = istr_mant.argumento[6] 
//ls_calibr = dw_1.GetItemString(il_fila, "pafr_calibr")
//ls_huert1 = String(dw_1.GetItemNumber(il_fila, "pafr_huert1"))
//ls_cuart1 = String(dw_1.GetItemNumber(il_fila, "pafr_cuart1"))
//
//ll_fila	= dw_1.Find( "clie_codigo = " + ls_client + &
//						    " AND paen_numero = " + ls_nropal + &
//						    " AND espe_codigo = " + ls_especi + &
//							 " AND vari_codigo = " + ls_varied + &
//							 " AND emba_codigo = '" + ls_embala + "'" + &
//							 " AND prod_codigo = " + ls_produc + &
//							 " AND cond_codigo = " + ls_condic + &
//							 " AND etiq_codigo = " + ls_etique + & 
//							 " AND plde_codigo = " + ls_planta + &
//							 " AND pafr_huert1 = " + ls_huert1 + &
//							 " AND pafr_cuart1 = " + ls_cuart1 + &
//							 " AND pafr_calibr = '" + ls_calibr + "'", 1, dw_1.RowCount())

//ll_fila	= dw_1.Find( "capr_numero = " + ls_numero, 1, dw_1.RowCount()) 

li_Cliente	=	Integer(istr_mant.Argumento[1])
li_Planta	=	Integer(istr_mant.Argumento[2])


ll_Fila	=	dw_2.Find("clie_codigo = " + String(li_Cliente) + &
						    " AND plde_codigo = " + String(li_Planta) + &
						    " AND capr_numero = " + String(al_Numero) , 1, &
						    dw_2.RowCount())


IF ll_fila > 0 and ll_fila <> il_fila THEN 
	MessageBox("Error","Calidad ya fue ingresada anteriormente",Information!, Ok!)
	lb_Retorno	=	True
ELSE
	lb_Retorno	=	False
END IF

RETURN lb_Retorno
	

end function

on w_mant_deta_generapalletdesdecajas_grane.create
int iCurrent
call super::create
this.dw_2=create dw_2
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_2
end on

on w_mant_deta_generapalletdesdecajas_grane.destroy
call super::destroy
destroy(this.dw_2)
end on

event ue_recuperadatos;call super::ue_recuperadatos;//ias_campo[13]	=	String(dw_1.Object.prod_codigo[il_fila])
//ias_campo[12]	=	dw_1.Object.pafr_calibr[il_fila]
//ias_campo[12]	=	dw_1.Object.capr_calibr[il_fila]
//ias_campo[11]	=	String(dw_1.Object.pafr_ccajas[il_fila])
//ias_campo[14]	=	String(dw_1.Object.pafr_nrlote[il_fila])
//
//IF istr_mant.agrega THEN
//	dw_1.SetItem(il_fila, "prod_codigo", dw_1.GetItemNumber(dw_1.GetRow(),'prod_codigo'))
//	dw_1.SetItem(il_fila, "clie_codigo", Integer(istr_mant.argumento[1]))
//	dw_1.SetItem(il_fila, "paen_numero", Long(istr_mant.argumento[2]))
//	dw_1.SetItem(il_fila, "espe_codigo", Integer(istr_mant.argumento[3]))
//	dw_1.SetItem(il_fila, "vari_codigo", Integer(istr_mant.argumento[4]))
//	dw_1.SetItem(il_fila, "variedades_vari_nombre", istr_mant.argumento[5])
//	dw_1.SetItem(il_fila, "vari_codigo", Integer(istr_mant.argumento[4]))
//	dw_1.SetItem(il_fila, "variedades_vari_nombre", istr_mant.argumento[5])
//	dw_1.SetItem(il_fila, "emba_codigo", istr_mant.argumento[7])
//	dw_1.SetItem(il_fila, "embalajesprod_emba_nombre", istr_mant.argumento[8])
//	dw_1.SetItem(il_fila, "cond_codigo", Integer(istr_mant.argumento[10]))
//	dw_1.SetItem(il_fila, "etiq_codigo", Integer(istr_mant.argumento[9]))
//	dw_1.SetItem(il_fila, "plde_codigo", Integer(istr_mant.argumento[6]))
//	dw_1.SetItem(il_fila, "pafr_fecemb", Date(istr_mant.argumento[40]))
//END IF

IF istr_mant.agrega = False and istr_mant.borra = False THEN
//	istr_mant.argumento[11]=String(Long(istr_mant.argumento[11])+Long(ias_campo[11]))
ELSE
//	dw_1.SetItem(il_fila, "pafr_ccajas", Integer(istr_mant.argumento[11]))
	dw_1.SetItem(il_fila,"caja_cajneo",istr_mant.argumento[37])		// Numero de Cajas
END IF


end event

event ue_deshace;call super::ue_deshace;dw_1.Object.prod_codigo[il_fila]	=	Long(ias_campo[13])
dw_1.Object.pafr_calibr[il_fila]	=	ias_campo[12]
dw_1.Object.pafr_ccajas[il_fila]	=	Integer(ias_campo[11])
dw_1.Object.pafr_nrlote[il_fila]	=	Integer(ias_campo[14])
end event

event ue_antesguardar;Long ll_Fila

ll_Fila = dw_2.InsertRow(0)

IF ll_Fila > 0 THEN
	dw_2.Object.clie_codigo[ll_Fila]	= dw_1.Object.clie_codigo[il_Fila]	
	dw_2.Object.plde_codigo[ll_Fila]	= dw_1.Object.plde_codigo[il_Fila]	
	dw_2.Object.capr_numero[ll_Fila]	= dw_1.Object.capr_numero[il_Fila]	
	dw_2.Object.espe_codigo[ll_Fila]	= dw_1.Object.espe_codigo[il_Fila]	
	dw_2.Object.vari_codigo[ll_Fila]	= dw_1.Object.vari_codigo[il_Fila]	
	dw_2.Object.prod_codigo[ll_Fila]	= dw_1.Object.prod_codigo[il_Fila]	
	dw_2.Object.prod_predio[ll_Fila]	= dw_1.Object.prod_predio[il_Fila]	
	dw_2.Object.prod_huerto[ll_Fila]	= dw_1.Object.prod_huerto[il_Fila]	
	dw_2.Object.prod_cuarte[ll_Fila]	= dw_1.Object.prod_cuarte[il_Fila]	
	dw_2.Object.emba_codigo[ll_Fila]	= dw_1.Object.emba_codigo[il_Fila]	
	dw_2.Object.etiq_codigo[ll_Fila]	= dw_1.Object.etiq_codigo[il_Fila]	
	dw_2.Object.capr_fecemb[ll_Fila]	= dw_1.Object.capr_fecemb[il_Fila]	
	dw_2.Object.capr_calibr[ll_Fila]	= dw_1.Object.capr_calibr[il_Fila]	
	dw_2.Object.capr_embala[ll_Fila]	= dw_1.Object.capr_embala[il_Fila]	
	dw_2.Object.capr_selecc[ll_Fila]	= dw_1.Object.capr_selecc[il_Fila]	
	dw_2.Object.capr_pesado[ll_Fila]	= dw_1.Object.capr_pesado[il_Fila]	
	dw_2.Object.capr_cean14[ll_Fila]	= dw_1.Object.capr_cean14[il_Fila]	
	dw_2.Object.capr_numpal[ll_Fila]	= dw_1.Object.capr_numpal[il_Fila]	
	dw_2.Object.capr_regcap[ll_Fila]	= dw_1.Object.capr_regcap[il_Fila]	
	dw_2.Object.capr_estado[ll_Fila]	= dw_1.Object.capr_estado[il_Fila]	
	dw_2.Object.capr_varrot[ll_Fila]	= dw_1.Object.capr_varrot[il_Fila]	
	dw_2.Object.capr_numgia[ll_Fila]	= dw_1.Object.capr_numgia[il_Fila]	
	dw_2.Object.cate_codigo[ll_Fila]	= dw_1.Object.cate_codigo[il_Fila]	
	dw_2.Object.capr_cespak[ll_Fila]	= dw_1.Object.capr_cespak[il_Fila]	
	dw_2.Object.capr_docrel[ll_Fila]	= dw_1.Object.capr_docrel[il_Fila]	
	dw_2.Object.capr_fecdig[ll_Fila]	= dw_1.Object.capr_fecdig[il_Fila]	
	dw_2.Object.capr_hordig[ll_Fila]	= dw_1.Object.capr_hordig[il_Fila]	
	dw_2.Object.capr_nrlote[ll_Fila]	= dw_1.Object.capr_nrlote[il_Fila]	
	dw_2.Object.capr_numtra[ll_Fila]	= dw_1.Object.capr_numtra[il_Fila]	
	
	il_Fila = ll_Fila
END IF
end event

event ue_nuevo;call super::ue_nuevo;
//IF Long(istr_mant.argumento[11])<1 THEN
//	MessageBox("Atención","Se ha completado las cajas del Pallet.")
//	dw_1.Enabled =  False
//	pb_acepta.SetFocus()
//ELSE
//	
//END IF

dw_1.SetColumn("caja_cajneo")
dw_1.SetFocus()

end event

event open;x	= 100
y	= 450

PostEvent("ue_recuperadatos")
istr_mant = Message.PowerObjectParm
//istr_mant.argumento[24] = istr_mant.argumento[24]
//
//IF istr_mant.argumento[16]='1' THEN
//	dw_1.Enabled	=	False
//END IF

IF istr_mant.solo_consulta = True THEN
	dw_1.Enabled	=	False
END IF

dw_1.SetTransObject(sqlca)

dw_1.GetChild("espe_codigo", dw_especies)
//dw_1.GetChild("vari_codigo", dw_variedades)
//dw_1.GetChild("emba_codigo", dw_embalajes)
dw_1.GetChild("etiq_codigo", dw_etiquetas)
dw_1.GetChild("plde_codigo", dw_plantadesp)
dw_1.GetChild("prod_codigo", dw_productor)
//dw_1.GetChild("pafr_calibr", dw_calibres)

dw_especies.SetTransObject(sqlca)
//dw_variedades.SetTransObject(sqlca)
//dw_embalajes.SetTransObject(sqlca)
dw_etiquetas.SetTransObject(sqlca)
dw_plantadesp.SetTransObject(sqlca)
dw_productor.SetTransObject(sqlca)
//dw_calibres.SetTransObject(sqlca)

dw_especies.Retrieve()

//dw_variedades.Retrieve(Integer(istr_mant.argumento[3]))
					  							  
////dw_embalajes.Retrieve(Integer(istr_mant.Argumento[1]))

dw_etiquetas.Retrieve()

dw_plantadesp.Retrieve(Integer(istr_mant.Argumento[6]))

dw_productor.Retrieve()

dw_1.SetFocus()

//istr_mant.dw.ShareData(dw_1)
dw_2.SetTransObject(sqlca)
istr_mant.dw.ShareData(dw_2)
end event

event ue_guardar;IF dw_1.AcceptText() = -1 THEN
	Message.DoubleParm	=	-1 
	RETURN
END IF

SetPointer(HourGlass!)

Message.DoubleParm	=	0
w_main.SetMicroHelp("Grabando información...")

TriggerEvent("ue_antesguardar")

SetPointer(Arrow!)

IF Message.DoubleParm = -1 THEN
	RETURN
ELSE
	w_maed_generapalletdesdecajas_granel.TriggerEvent("ue_guardar")
END IF


end event

type pb_ultimo from w_mant_detalle_csd`pb_ultimo within w_mant_deta_generapalletdesdecajas_grane
boolean visible = false
end type

type pb_siguiente from w_mant_detalle_csd`pb_siguiente within w_mant_deta_generapalletdesdecajas_grane
boolean visible = false
end type

type pb_anterior from w_mant_detalle_csd`pb_anterior within w_mant_deta_generapalletdesdecajas_grane
boolean visible = false
end type

type pb_primero from w_mant_detalle_csd`pb_primero within w_mant_deta_generapalletdesdecajas_grane
boolean visible = false
end type

type pb_cancela from w_mant_detalle_csd`pb_cancela within w_mant_deta_generapalletdesdecajas_grane
integer x = 2405
integer y = 372
integer taborder = 20
end type

type pb_acepta from w_mant_detalle_csd`pb_acepta within w_mant_deta_generapalletdesdecajas_grane
integer x = 2405
integer y = 160
integer taborder = 10
end type

event pb_acepta::clicked;istr_mant.respuesta = 1

IF istr_mant.agrega THEN
	Parent.TriggerEvent("ue_nuevo")
ELSE
	CloseWithReturn(Parent, istr_mant)
END IF
end event

type pb_salir from w_mant_detalle_csd`pb_salir within w_mant_deta_generapalletdesdecajas_grane
integer x = 2405
integer y = 592
integer taborder = 30
end type

type dw_1 from w_mant_detalle_csd`dw_1 within w_mant_deta_generapalletdesdecajas_grane
integer x = 9
integer y = 12
integer width = 2309
integer height = 1496
integer taborder = 0
string dataobject = "dw_mant_spro_cajasprod"
end type

event dw_1::itemchanged;Integer 	li_Cliente, li_Planta, li_busca
String	ls_columna, ls_nula, ls_Data
Long		ll_numcaja, ll_NroCaja
SetNull(ls_Nula)

ls_columna = dwo.name

CHOOSE CASE ls_columna
	CASE "caja_cajneo"
		IF Len(Data) < 16 THEN
			MessageBox("Error", "Ha leído un Código~rde Barra Inválido.", &
							StopSign!)
			This. SetItem(il_Fila,ls_Columna,ls_Nula)
			RETURN 1
		END IF 
		 
		ls_Data		=	Data
		li_Planta	=	Integer(Mid(Data, 3, 4))
		ll_NroCaja	=	Long(Mid(Data, 7, 10))			
		li_Cliente	=	Integer(istr_mant.Argumento[1])
		//li_Planta	=	Integer(istr_mant.Argumento[2])
		//This.SetItem(il_fila,'capr_numero',ll_NroCaja)
				
   	IF NoExisteCaja(ll_NroCaja) OR Duplicado(ll_NroCaja) THEN							
			This.SetItem(il_Fila, 'caja_cajneo', ls_Nula)
			dw_1.SetColumn("caja_cajneo")
			dw_1.SetFocus()	
			RETURN 1
		ELSE
			dw_1.Object.caja_cajneo[1]	=	ls_Data
		   pb_acepta.PostEvent(Clicked!)			
		END IF
END CHOOSE
end event

type dw_2 from datawindow within w_mant_deta_generapalletdesdecajas_grane
boolean visible = false
integer x = 2126
integer y = 1360
integer width = 686
integer height = 400
integer taborder = 40
boolean bringtotop = true
string title = "none"
string dataobject = "dw_mues_cajasprodpallet"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

