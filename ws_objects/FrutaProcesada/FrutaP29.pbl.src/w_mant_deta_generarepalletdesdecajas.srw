$PBExportHeader$w_mant_deta_generarepalletdesdecajas.srw
$PBExportComments$Mantención Detalle de Despacho Pallets
forward
global type w_mant_deta_generarepalletdesdecajas from w_mant_detalle
end type
type dw_4 from datawindow within w_mant_deta_generarepalletdesdecajas
end type
type rb_caja from radiobutton within w_mant_deta_generarepalletdesdecajas
end type
type rb_pallet from radiobutton within w_mant_deta_generarepalletdesdecajas
end type
type dw_3 from datawindow within w_mant_deta_generarepalletdesdecajas
end type
type dw_2 from datawindow within w_mant_deta_generarepalletdesdecajas
end type
end forward

global type w_mant_deta_generarepalletdesdecajas from w_mant_detalle
integer width = 2688
integer height = 1768
dw_4 dw_4
rb_caja rb_caja
rb_pallet rb_pallet
dw_3 dw_3
dw_2 dw_2
end type
global w_mant_deta_generarepalletdesdecajas w_mant_deta_generarepalletdesdecajas

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
public function boolean duplicado (long al_numero)
public function boolean noexistepallet (long al_numero)
public subroutine cargacaja (long al_fila)
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
			li_enc_estado,li_enc_PCoPDA, li_Existe
Long		ll_numpal,ll_docrel,ll_productor,ll_numgia,ll_numcaja,ll_nrlote,ll_numtra
Date		ld_fecha,ld_fecemb, ld_fecdig
String	ls_embala,ls_calibr,ls_cean14,ls_regcap,ls_productor
Boolean	lb_Retorno
Time		lt_hordig

lb_Retorno	=	False

li_Cliente	=	Integer(istr_mant.argumento[1])
li_Planta	=	Integer(istr_mant.argumento[6])
ll_numcaja	=	al_numero

SELECT capr_docrel,capr_fecemb,prod_codigo,espe_codigo,vari_codigo,
		prod_predio,prod_huerto,prod_cuarte,emba_codigo,etiq_codigo,
		capr_calibr,capr_embala,capr_selecc,capr_pesado,
		capr_cean14,capr_numpal,capr_regcap,capr_estado,capr_varrot,
		capr_numgia,cate_codigo,capr_cespak,capr_nrlote,
		capr_fecdig,capr_hordig,capr_nrlote,capr_numtra
		INTO	:ll_docrel,:ld_fecha,:ll_productor,:li_especie,:li_variedad,
		:li_predio,:li_huerto,:li_cuarte,:ls_embala,:li_etique,
		:ls_calibr,:li_embdor,:li_selecc,:li_pesado,
		:ls_cean14,:ll_numpal,:ls_regcap,:li_estado,:li_varrot,
		:ll_numgia,:li_catego,:li_cespak,:li_nrlote,
		:ld_fecdig,:lt_hordig,:ll_nrlote,:ll_numtra
		FROM dba.spro_cajasprodPallet
		WHERE clie_codigo = :li_Cliente
		AND   plde_codigo = :li_Planta
		AND   capr_numero = :al_numero;

IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura tabla spro_cajasprodPallet")
	lb_Retorno	=	True
ELSEIF sqlca.SQLCode = 100 THEN
		 MessageBox("Cuidado","Número de Caja No Existe en Tabla Respectiva!! ", &
							StopSign!)	
       lb_Retorno	=	True
END IF

IF IsNull(ll_numpal) OR ll_numpal <= 0 THEN
	MessageBox("Cuidado","Número de Caja No esta Palletizada !! ", &
	StopSign!) 
	lb_Retorno	=	True
END IF

IF lb_Retorno	=	False THEN
	SELECT Count(*)
	INTO  :li_Existe
	FROM dba.palletencab
	WHERE clie_codigo = :li_Cliente
	AND   plde_codigo = :li_Planta
	AND   paen_numero = :ll_numpal;
	
	IF sqlca.SQLCode = -1 THEN
		F_ErrorBaseDatos(sqlca, "Lectura tabla palletencab")
		lb_Retorno	=	True
	ELSEIF sqlca.SQLCode = 100 THEN
			 MessageBox("Cuidado","Caja No ha sido Recepcionada en Frigorífico!! ", &
								StopSign!)	
			 lb_Retorno	=	True
	ELSEIF li_Existe > 0 THEN
			SELECT paen_estado, paen_pcopda
			INTO  :li_enc_estado, :li_enc_PCoPDA
			FROM dba.palletencab
			WHERE clie_codigo = :li_Cliente
			AND   plde_codigo = :li_Planta
			AND   paen_numero = :ll_numpal;
			
			IF li_enc_estado = 2 THEN
				MessageBox("Error","El Nro. Pallet Asociado a la Caja No está en Existencia", &
								Information!, Ok!)
				lb_Retorno	=	True
			END IF
			
			IF Isnull(li_enc_PCoPDA) THEN
				MessageBox("Error","El Nro. Pallet Asociado a la Caja No es Ingreso Caja a Caja", &
								Information!, Ok!)
				lb_Retorno	=	True
			END IF
			
			dw_1.Object.clie_codigo[il_Fila]	=	li_Cliente
			dw_1.Object.plde_codigo[il_Fila]	=	li_Planta
			dw_1.Object.capr_numero[il_Fila]	=	al_numero
			dw_1.Object.capr_fecemb[il_Fila]	=	ld_fecha
			dw_1.Object.prod_codigo[il_Fila]	=	ll_productor
			dw_1.Object.espe_codigo[il_Fila]	=	li_especie
			dw_1.Object.vari_codigo[il_Fila]	=	li_variedad
			dw_1.Object.prod_predio[il_Fila]	=	li_predio
			dw_1.Object.prod_huerto[il_Fila]	=	li_huerto
			dw_1.Object.prod_cuarte[il_Fila]	=	li_cuarte
			dw_1.Object.emba_codigo[il_Fila]	=	ls_embala
			dw_1.Object.etiq_codigo[il_Fila]	=	li_etique
			dw_1.Object.capr_calibr[il_Fila]	=	ls_calibr
			dw_1.Object.capr_embala[il_Fila]	=	li_embdor
			dw_1.Object.capr_selecc[il_Fila]	=	li_selecc
			dw_1.Object.capr_pesado[il_Fila]	=	li_pesado
			dw_1.Object.capr_cean14[il_Fila]	=	ls_cean14
			dw_1.Object.capr_numpal[il_Fila]	=	ll_numpal
			dw_1.Object.capr_regcap[il_Fila]	=	ls_regcap
			dw_1.Object.capr_estado[il_Fila]	=	li_estado
			dw_1.Object.capr_varrot[il_Fila]	=	li_varrot
			dw_1.Object.capr_numgia[il_Fila]	=	ll_numgia
			dw_1.Object.cate_codigo[il_Fila]	=	li_catego
			dw_1.Object.capr_cespak[il_Fila]	=	li_cespak
			dw_1.Object.capr_docrel[il_Fila]	=	ll_docrel
			dw_1.Object.capr_fecdig[il_Fila]	=	ld_fecdig
			dw_1.Object.capr_hordig[il_Fila]	=	lt_hordig
			dw_1.Object.capr_nrlote[il_Fila]	=	ll_nrlote
			dw_1.Object.capr_numtra[il_Fila]	=	ll_numtra
	
			//// Para la primera vez.
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
							lb_Retorno	=	False
					ELSE		
							lb_Retorno	=	True
					END IF		
				END IF	
			END IF
	ELSE
		 MessageBox("Cuidado","Caja No ha sido Recepcionada en Frigorífico!! ", &
						StopSign!)	
		 lb_Retorno	=	True
	END IF
END IF

RETURN lb_Retorno

end function

public function boolean duplicado (long al_numero);//Long		ll_fila,ll_numcaja
//String		ls_numero
//
//ll_numcaja	=	Long(Trim(campo))
//ls_numero	=	campo
//
//ll_fila	= dw_1.Find( "capr_numero = " + ls_numero, 1, dw_1.RowCount())
//
//IF ll_fila > 0 and ll_fila <> il_fila THEN
//	MessageBox("Error","La Caja ya existe en el Pallet",Information!, Ok!)
//	RETURN True
//ELSE
//	RETURN False
//END IF

Long		ll_Fila
Integer	li_Cliente, li_Planta

li_Cliente	=	Integer(istr_mant.Argumento[1])
li_Planta	=	Integer(istr_mant.Argumento[2])

ll_Fila	=	dw_1.Find("clie_codigo = " + String(li_Cliente) + &
						    " AND plde_codigo = " + String(li_Planta) + &
						    " AND capr_numero = " + String(al_Numero) , 1, &
						    dw_1.RowCount())
	
//IF ll_Fila > 0 AND ll_Fila <> il_Fila THEN
IF ll_Fila > 0 THEN
				MessageBox("Error","Caja ya fue incluida en Detalle del Pallet", &
					Information!, Ok!)
					
				RETURN True
END IF

RETURN False
end function

public function boolean noexistepallet (long al_numero);Boolean	lb_Retorno
Integer 	li_Cliente, li_Planta, li_enc_estado, li_enc_PCoPDA, li_Existe, li_Fila
Long		ll_numPal, ll_Cajas

li_Cliente	=	Integer(istr_mant.argumento[1])
li_Planta	=	Integer(istr_mant.argumento[6])
ll_numPal	=	al_numero

ll_Cajas		= 0


SELECT Count(*)
	INTO :li_Existe
	FROM DBA.spro_cajasprodPallet
	WHERE clie_codigo = :li_Cliente
	AND   plde_codigo = :li_Planta
	AND   capr_numpal = :ll_numPal;	

IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura tabla spro_cajasprodPallet")
	lb_Retorno	=	False
ELSEIF sqlca.SQLCode = 100 THEN
		 MessageBox("Cuidado","Número de Pallet No Asociado a Cajas!! ", &
							StopSign!)	
			lb_Retorno	=	False
ELSEIF li_Existe > 0 THEN

		SELECT Count(*)
		INTO  :li_Existe
		FROM dba.palletencab
		WHERE clie_codigo = :li_Cliente
		AND   plde_codigo = :li_Planta
		AND   paen_numero = :ll_numPal;
		
		IF sqlca.SQLCode = -1 THEN
			F_ErrorBaseDatos(sqlca, "Lectura tabla palletencab")
			lb_Retorno	=	False
		ELSEIF sqlca.SQLCode = 100 THEN
				 MessageBox("Cuidado","Pallet No ha sido Recepcionado en Frigorífico!! ", &
									StopSign!)	
				 lb_Retorno	=	False
		ELSEIF li_Existe > 0 THEN

			SELECT paen_estado, paen_pcopda
			INTO  :li_enc_estado, :li_enc_PCoPDA
			FROM dba.palletencab
			WHERE clie_codigo = :li_Cliente
			AND   plde_codigo = :li_Planta
			AND   paen_numero = :ll_numPal;
			
			IF li_enc_estado = 2 OR li_enc_estado = 3 THEN
				MessageBox("Error","El Nro. Pallet Asociado a la Caja No está en Existencia", &
								Information!, Ok!)
				lb_Retorno	=	False
			END IF
			
			IF Isnull(li_enc_PCoPDA) THEN
				MessageBox("Error","El Nro. Pallet Asociado a la Caja No es Ingreso Caja a Caja", &
								Information!, Ok!)
				lb_Retorno	=	False
			END IF
			
			dw_2.Retrieve(li_Planta,ll_numPal,li_Cliente)
			IF dw_2.RowCount() > 0 THEN
			ELSE
				MessageBox("Atención","El Nro. Pallet No tiene Ninguna Caja Asociada.", &
								Information!, Ok!)
				lb_Retorno	=	False
			END IF
			
			FOR li_Fila	=	1 TO dw_2.RowCount()
				dw_2.Object.selecc[li_Fila] = 1
				ll_Cajas++
			NEXT
			dw_3.Object.ccajas[1] = ll_Cajas
			
			lb_Retorno	=	True	
		ELSE
			 MessageBox("Cuidado","Pallet No ha sido Recepcionado en Frigorífico!! ", &
				StopSign!)	
		    lb_Retorno	=	False
		END IF
ELSE
	MessageBox("Atención","El Nro. Pallet No tiene Ninguna Caja Asociada.", &
				Information!, Ok!)
	lb_Retorno	=	False
END IF

RETURN lb_Retorno
end function

public subroutine cargacaja (long al_fila);
dw_1.Object.clie_codigo[il_Fila]	=	dw_2.Object.clie_codigo[al_Fila]
dw_1.Object.plde_codigo[il_Fila]	=	dw_2.Object.plde_codigo[al_Fila]
dw_1.Object.capr_numero[il_Fila]	=	dw_2.Object.capr_numero[al_Fila]
dw_1.Object.capr_fecemb[il_Fila]	=	dw_2.Object.capr_fecemb[al_Fila]
dw_1.Object.prod_codigo[il_Fila]	=	dw_2.Object.prod_codigo[al_Fila]
dw_1.Object.espe_codigo[il_Fila]	=	dw_2.Object.espe_codigo[al_Fila]
dw_1.Object.vari_codigo[il_Fila]	=	dw_2.Object.vari_codigo[al_Fila]
dw_1.Object.prod_predio[il_Fila]	=	dw_2.Object.prod_predio[al_Fila]
dw_1.Object.prod_huerto[il_Fila]	=	dw_2.Object.prod_huerto[al_Fila]
dw_1.Object.prod_cuarte[il_Fila]	=	dw_2.Object.prod_cuarte[al_Fila]
dw_1.Object.emba_codigo[il_Fila]	=	dw_2.Object.emba_codigo[al_Fila]
dw_1.Object.etiq_codigo[il_Fila]	=	dw_2.Object.etiq_codigo[al_Fila]
dw_1.Object.capr_calibr[il_Fila]	=	dw_2.Object.capr_calibr[al_Fila]
dw_1.Object.capr_embala[il_Fila]	=	dw_2.Object.capr_embala[al_Fila]
dw_1.Object.capr_selecc[il_Fila]	=	dw_2.Object.capr_selecc[al_Fila]
dw_1.Object.capr_pesado[il_Fila]	=	dw_2.Object.capr_pesado[al_Fila]
dw_1.Object.capr_cean14[il_Fila]	=	dw_2.Object.capr_cean14[al_Fila]
dw_1.Object.capr_numpal[il_Fila]	=	dw_2.Object.capr_numpal[al_Fila]
dw_1.Object.capr_regcap[il_Fila]	=	dw_2.Object.capr_regcap[al_Fila]
dw_1.Object.capr_estado[il_Fila]	=	dw_2.Object.capr_estado[al_fila]
dw_1.Object.capr_varrot[il_Fila]	=	dw_2.Object.capr_varrot[al_Fila]
dw_1.Object.capr_numgia[il_Fila]	=	dw_2.Object.capr_numgia[al_Fila]
dw_1.Object.cate_codigo[il_Fila]	=	dw_2.Object.cate_codigo[al_Fila]
dw_1.Object.capr_cespak[il_Fila]	=	dw_2.Object.capr_cespak[al_Fila]
dw_1.Object.capr_docrel[il_Fila]	=  dw_2.Object.capr_docrel[al_Fila]	
dw_1.Object.capr_fecdig[il_Fila]	=  dw_2.Object.capr_fecdig[al_Fila]	
dw_1.Object.capr_hordig[il_Fila]	=  dw_2.Object.capr_hordig[al_Fila]	
dw_1.Object.capr_nrlote[il_Fila]	=  dw_2.Object.capr_nrlote[al_Fila]	
dw_1.Object.capr_numtra[il_Fila]	=  dw_2.Object.capr_numtra[al_Fila]	

end subroutine

on w_mant_deta_generarepalletdesdecajas.create
int iCurrent
call super::create
this.dw_4=create dw_4
this.rb_caja=create rb_caja
this.rb_pallet=create rb_pallet
this.dw_3=create dw_3
this.dw_2=create dw_2
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_4
this.Control[iCurrent+2]=this.rb_caja
this.Control[iCurrent+3]=this.rb_pallet
this.Control[iCurrent+4]=this.dw_3
this.Control[iCurrent+5]=this.dw_2
end on

on w_mant_deta_generarepalletdesdecajas.destroy
call super::destroy
destroy(this.dw_4)
destroy(this.rb_caja)
destroy(this.rb_pallet)
destroy(this.dw_3)
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

dw_1.SetColumn("caja_cajneo")


end event

event ue_deshace;call super::ue_deshace;dw_1.Object.prod_codigo[il_fila]	=	Long(ias_campo[13])
dw_1.Object.pafr_calibr[il_fila]	=	ias_campo[12]
dw_1.Object.pafr_ccajas[il_fila]	=	Integer(ias_campo[11])
dw_1.Object.pafr_nrlote[il_fila]	=	Integer(ias_campo[14])
end event

event ue_antesguardar;Long ll_Fila

ll_Fila = dw_4.InsertRow(0)

IF ll_Fila > 0 THEN
	dw_4.Object.clie_codigo[ll_Fila]	= dw_1.Object.clie_codigo[il_Fila]	
	dw_4.Object.plde_codigo[ll_Fila]	= dw_1.Object.plde_codigo[il_Fila]	
	dw_4.Object.capr_numero[ll_Fila]	= dw_1.Object.capr_numero[il_Fila]	
	dw_4.Object.espe_codigo[ll_Fila]	= dw_1.Object.espe_codigo[il_Fila]	
	dw_4.Object.vari_codigo[ll_Fila]	= dw_1.Object.vari_codigo[il_Fila]	
	dw_4.Object.prod_codigo[ll_Fila]	= dw_1.Object.prod_codigo[il_Fila]	
	dw_4.Object.prod_predio[ll_Fila]	= dw_1.Object.prod_predio[il_Fila]	
	dw_4.Object.prod_huerto[ll_Fila]	= dw_1.Object.prod_huerto[il_Fila]	
	dw_4.Object.prod_cuarte[ll_Fila]	= dw_1.Object.prod_cuarte[il_Fila]	
	dw_4.Object.emba_codigo[ll_Fila]	= dw_1.Object.emba_codigo[il_Fila]	
	dw_4.Object.etiq_codigo[ll_Fila]	= dw_1.Object.etiq_codigo[il_Fila]	
	dw_4.Object.capr_fecemb[ll_Fila]	= dw_1.Object.capr_fecemb[il_Fila]	
	dw_4.Object.capr_calibr[ll_Fila]	= dw_1.Object.capr_calibr[il_Fila]	
	dw_4.Object.capr_embala[ll_Fila]	= dw_1.Object.capr_embala[il_Fila]	
	dw_4.Object.capr_selecc[ll_Fila]	= dw_1.Object.capr_selecc[il_Fila]	
	dw_4.Object.capr_pesado[ll_Fila]	= dw_1.Object.capr_pesado[il_Fila]	
	dw_4.Object.capr_cean14[ll_Fila]	= dw_1.Object.capr_cean14[il_Fila]	
	dw_4.Object.capr_numpal[ll_Fila]	= dw_1.Object.capr_numpal[il_Fila]	
	dw_4.Object.capr_regcap[ll_Fila]	= dw_1.Object.capr_regcap[il_Fila]	
	dw_4.Object.capr_estado[ll_Fila]	= dw_1.Object.capr_estado[il_Fila]	
	dw_4.Object.capr_varrot[ll_Fila]	= dw_1.Object.capr_varrot[il_Fila]	
	dw_4.Object.capr_numgia[ll_Fila]	= dw_1.Object.capr_numgia[il_Fila]	
	dw_4.Object.cate_codigo[ll_Fila]	= dw_1.Object.cate_codigo[il_Fila]	
	dw_4.Object.capr_cespak[ll_Fila]	= dw_1.Object.capr_cespak[il_Fila]	
	dw_4.Object.capr_docrel[ll_Fila]	= dw_1.Object.capr_docrel[il_Fila]	
	dw_4.Object.capr_fecdig[ll_Fila]	= dw_1.Object.capr_fecdig[il_Fila]	
	dw_4.Object.capr_hordig[ll_Fila]	= dw_1.Object.capr_hordig[il_Fila]	
	dw_4.Object.capr_nrlote[ll_Fila]	= dw_1.Object.capr_nrlote[il_Fila]	
	dw_4.Object.capr_numtra[ll_Fila]	= dw_1.Object.capr_numtra[il_Fila]	
	
	il_Fila = ll_Fila
END IF
end event

event ue_nuevo;call super::ue_nuevo;
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

//dw_calibres.Retrieve(Integer(istr_mant.Argumento[3]), &
//							Integer(istr_mant.Argumento[4]))


dw_1.SetFocus()

dw_4.SetTransObject(sqlca)
istr_mant.dw.ShareData(dw_4)

dw_2.SetTransObject(sqlca)
dw_3.SetTransObject(sqlca)


end event

event resize;//This.Height			= dw_1.Height + 232
//This.Width			= dw_1.width + 540

pb_acepta.y			= 300
pb_acepta.width	= 156
pb_acepta.height	= 133

pb_cancela.x		= pb_acepta.x
pb_cancela.y		= pb_acepta.y + 180
pb_cancela.width	= 156
pb_cancela.height	= 133

pb_salir.x			= pb_acepta.x
pb_salir.y			= pb_cancela.y + 180
pb_salir.width		= 156
pb_salir.height	= 133

end event

event ue_guardar;IF dw_1.AcceptText() = -1 THEN
	Message.DoubleParm	=	-1 
	RETURN
END IF

SetPointer(HourGlass!)

Message.DoubleParm	=	0

TriggerEvent("ue_antesguardar")

SetPointer(Arrow!)

IF Message.DoubleParm = -1 THEN
	RETURN
ELSE
	w_maed_generarepalletdesdecajas.TriggerEvent("ue_guardar")
END IF
end event

type pb_ultimo from w_mant_detalle`pb_ultimo within w_mant_deta_generarepalletdesdecajas
boolean visible = false
end type

type pb_siguiente from w_mant_detalle`pb_siguiente within w_mant_deta_generarepalletdesdecajas
boolean visible = false
end type

type pb_anterior from w_mant_detalle`pb_anterior within w_mant_deta_generarepalletdesdecajas
boolean visible = false
end type

type pb_primero from w_mant_detalle`pb_primero within w_mant_deta_generarepalletdesdecajas
boolean visible = false
end type

type pb_cancela from w_mant_detalle`pb_cancela within w_mant_deta_generarepalletdesdecajas
integer x = 2405
integer y = 372
integer taborder = 20
end type

type pb_acepta from w_mant_detalle`pb_acepta within w_mant_deta_generarepalletdesdecajas
integer x = 2405
integer y = 160
integer taborder = 10
end type

event pb_acepta::clicked;//istr_mant.respuesta = 1
//
//IF istr_mant.agrega THEN
//	Parent.TriggerEvent("ue_nuevo")
//ELSE
//	CloseWithReturn(Parent, istr_mant)
//END IF

Integer li_Fila

istr_mant.Respuesta = 1

IF istr_mant.agrega THEN
	IF rb_caja.Checked	=	True THEN
		IF Not IsNull(dw_1.Object.caja_cajneo[il_Fila]) THEN 
			Parent.TriggerEvent("ue_nuevo")
		END IF
	ELSEIF rb_pallet.Checked = True THEN
			IF dw_2.rowcount() > 0 THEN
				FOR li_Fila = 1 TO dw_2.RowCount()
					IF dw_2.Object.selecc[li_Fila] = 1 THEN					
						CargaCaja(li_Fila)
						Parent.TriggerEvent("ue_nuevo")						
					END IF
				NEXT
				dw_2.Reset()
				dw_3.Reset()
				dw_3.InsertRow(0)
				dw_3.SetColumn("paen_palneo")
				dw_3.SetFocus()
			END IF		
	END IF	
ELSE
	CloseWithReturn(Parent, istr_mant)
END IF


end event

type pb_salir from w_mant_detalle`pb_salir within w_mant_deta_generarepalletdesdecajas
integer x = 2405
integer y = 592
integer taborder = 30
end type

type dw_1 from w_mant_detalle`dw_1 within w_mant_deta_generarepalletdesdecajas
integer x = 9
integer y = 132
integer width = 2309
integer height = 1500
integer taborder = 0
string dataobject = "dw_mant_spro_cajasprod_repa"
end type

event dw_1::itemchanged;Integer 	li_Cliente, li_Planta, li_busca
String	ls_columna, ls_nula, ls_Data
Long		ll_NroCaja
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
		
		ls_Data		=	Trim(Data)
		li_Planta	=	Integer(Mid(ls_Data, 3, 4))
		ll_NroCaja	=	Long(Mid(ls_Data, 7, 10))		
				
		li_Cliente	=	Integer(istr_mant.Argumento[1])
						
   	IF NoExisteCaja(ll_NroCaja) OR Duplicado(ll_NroCaja) THEN							
			This.SetItem(il_Fila, 'caja_cajneo', ls_Nula)
			dw_1.SetColumn("caja_cajneo")
			dw_1.SetFocus()	
			RETURN 1			
		ELSE	
		   dw_1.Object.caja_cajneo[1]	=	ls_Data
			IF rb_caja.Checked	=	True THEN
			   pb_acepta.PostEvent(Clicked!)
			END IF
		END IF				
				
//	CASE "caja_cajneo"
//
//		li_Cliente	=	Integer(istr_mant.Argumento[1])
//		li_Planta	=	Integer(istr_mant.Argumento[6])
//		ll_numcaja	=	Long(Trim(data))
//		This.SetItem(il_fila,'capr_numero',ll_numcaja)
//		
//		IF Duplicado(Upper(data)) THEN
//			This.SetItem(il_fila, ls_Columna, ls_Nula)
//			RETURN 1
//		END IF
//				
//  		IF NoExisteCaja(ll_numcaja) THEN
//			This.SetItem(il_fila, ls_Columna, ls_Nula)
//			RETURN 1				
//		END If
END CHOOSE
end event

type dw_4 from datawindow within w_mant_deta_generarepalletdesdecajas
boolean visible = false
integer x = 2176
integer y = 1320
integer width = 686
integer height = 400
integer taborder = 60
boolean bringtotop = true
string title = "none"
string dataobject = "dw_mues_cajasprodpallet_repa"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type rb_caja from radiobutton within w_mant_deta_generarepalletdesdecajas
integer x = 1458
integer y = 28
integer width = 242
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
string text = "Caja"
boolean checked = true
end type

event clicked;IF rb_caja.Checked = True THEN
	dw_1.Visible = True
	dw_1.SetColumn("caja_cajneo")
	dw_1.SetFocus()
END IF
end event

type rb_pallet from radiobutton within w_mant_deta_generarepalletdesdecajas
integer x = 1861
integer y = 28
integer width = 242
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
string text = "Pallet"
end type

event clicked;IF rb_pallet.Checked = True THEN
	//dw_3.InsertRow(0)	
	dw_2.Visible = True
	dw_3.Visible = True
	dw_2.Reset()
	dw_3.Reset()
	dw_3.InsertRow(0)
	dw_3.Object.ccajas[1] = 0
	dw_3.SetColumn("paen_palneo")
	dw_3.SetFocus()
END IF
end event

type dw_3 from datawindow within w_mant_deta_generarepalletdesdecajas
integer x = 14
integer y = 136
integer width = 2304
integer height = 256
integer taborder = 50
string title = "none"
string dataobject = "dw_mant_spro_cajasprod_pallet"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event itemchanged;String	ls_Columna, ls_Nula
Date		ld_Nula
Integer	li_Nula, li_Cliente
Long		ll_NroPallet

SetNull(ls_Nula)
SetNull(li_Nula)
SetNull(ld_Nula)

ls_Columna	=	dwo.Name

CHOOSE CASE ls_Columna
		
	CASE "paen_palneo"
		IF Len(Data) < 5  THEN
			MessageBox("Error", "Ha leído un Código~rde Barra Inválido.", &
							StopSign!)
			This.SetItem(1, ls_Columna, ls_Nula)
			RETURN 1			
		END IF
		
		IF Len(data) > 8 THEN
			li_Cliente		=	Integer(Mid(Data, 1, Len(Data) - 7))
			ll_NroPallet	=	Long(Mid(Data, Len(Data) - 6))
			
			IF li_Cliente <> Integer(istr_mant.argumento[1]) THEN
			   Messagebox("Cuidado", "Código de Cliente Distinto a Encabezado")
				This.SetItem(1, ls_Columna, ls_Nula)
				RETURN 1				
			ELSE
				//dw_2.Object.paen_tipopa[1]	=	1
			END IF
		ELSE
			li_Cliente		=	0
			ll_NroPallet	=	Long(Data)
			//dw_2.Object.paen_tipopa[1]	=	2	
		END IF

		//dw_3.Object.paen_numero[1] = 	ll_NroPallet		
		//istr_mant.argumento[3]		=	String(ll_NroPallet)
				
		IF Not NoExistePallet(ll_NroPallet) THEN
			This.SetItem(1, ls_Columna, ls_Nula)
			RETURN 1			
		END IF
		
END CHOOSE


end event

type dw_2 from datawindow within w_mant_deta_generarepalletdesdecajas
integer x = 14
integer y = 392
integer width = 2304
integer height = 1240
integer taborder = 40
string title = "none"
string dataobject = "dw_mant_spro_cajasprodpallet"
boolean hscrollbar = true
boolean vscrollbar = true
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

