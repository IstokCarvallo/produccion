$PBExportHeader$w_mant_deta_palletfruta_interfecemb_rotulatrans.srw
forward
global type w_mant_deta_palletfruta_interfecemb_rotulatrans from w_mant_detalle_csd
end type
end forward

global type w_mant_deta_palletfruta_interfecemb_rotulatrans from w_mant_detalle_csd
integer width = 3959
integer height = 1744
end type
global w_mant_deta_palletfruta_interfecemb_rotulatrans w_mant_deta_palletfruta_interfecemb_rotulatrans

type variables
DataWindowChild   dw_calibres		,	dw_productor 	,	&
                  dw_embalajes	,	dw_variedades 	,	&
                  dw_plantadesp	,	dw_especies		,	&
                  dw_etiquetas	,	dw_condiciones	,	dw_cliente,&
						idwc_predio		, 	idwc_cuartel	, 	idw_ptaori,&
						idwc_calrot		,	idwc_prodrot	, 	idwc_embarot,&	
						idwc_valrot		,	idwc_prediorot ,	idwc_cuartelrot,	idwc_categoria,	idwc_caterotula
						
Integer il_lote						


end variables

forward prototypes
public function boolean existevariecab (integer as_valor)
public function boolean noexistecalibre (string as_valor)
public function boolean duplicado (string campo)
public function boolean noexisteproductor (string ls_columna)
public subroutine buscaprod ()
public function boolean varificaproductor (long ll_productor)
public function boolean noexistepredio (long ai_productor, integer ai_predio)
public function boolean noexistepacking (integer ai_packing)
public function boolean noexisteproductorrotulado (string ls_columna)
public function boolean noexistecuartel (long al_productor, integer ai_predio, integer ai_cuartel, integer ai_especie, integer ai_variedad)
public function boolean noexistecategoria (integer categoria)
end prototypes

public function boolean existevariecab (integer as_valor);Integer	li_especie, li_variedad, li_cliente
String	ls_nombre, ls_calibre
			//ls_secacod

li_cliente					= integer(istr_mant.argumento[1])
li_especie					= integer(istr_mant.argumento[3])
li_variedad					= as_valor

//ls_secacod					= istr_mant.argumento[12]

SELECT	vaca_calibr    INTO : ls_calibre
	FROM	dbo.variecalibre
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
	FROM  dbo.variecalibre
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

public function boolean duplicado (string campo);Long		ll_fila
String	ls_client, ls_nropal, ls_especi, ls_varied, ls_embala, ls_produc, ls_condic, ls_etique, &
			ls_planta, ls_zonaco, ls_calibr, ls_huert1, ls_cuart1, ls_copack, ls_fecemb
Date		ld_fecemb
ls_client = istr_mant.argumento[1]
ls_nropal = istr_mant.argumento[2]
ls_especi = istr_mant.argumento[3]
ls_varied = istr_mant.argumento[4]
ls_embala = istr_mant.argumento[7]
ls_produc = String(dw_1.GetItemNumber(il_fila, "prod_codigo"))
ls_condic = istr_mant.argumento[10]
ls_etique = istr_mant.argumento[9]
ls_planta = istr_mant.argumento[6]
ls_calibr = dw_1.GetItemString(il_fila, "pafr_calibr")
ls_huert1 = String(dw_1.GetItemNumber(il_fila, "pafr_huert1"))
ls_cuart1 = String(dw_1.GetItemNumber(il_fila, "pafr_cuart1"))
ls_copack = String(dw_1.GetItemNumber(il_fila, "pafr_copack"))
ls_fecemb = String(dw_1.GetItemDate(il_fila, "pafr_fecemb"))
ld_fecemb = dw_1.GetItemDate(il_fila, "pafr_fecemb")

ll_fila	= dw_1.Find( "clie_codigo = " + ls_client + &
						    " AND paen_numero = " + ls_nropal + &
						    " AND espe_codigo = " + ls_especi + &
							 " AND vari_codigo = " + ls_varied + &
							 " AND emba_codigo = '" + ls_embala + "'" + &
							 " AND prod_codigo = " + ls_produc + &
							 " AND cond_codigo = " + ls_condic + &
							 " AND etiq_codigo = " + ls_etique + &
							 " AND plde_codigo = " + ls_planta + &
							 " AND pafr_huert1 = " + ls_huert1 + &
							 " AND pafr_cuart1 = " + ls_cuart1 + &
							 " AND pafr_copack = " + ls_copack + &		
							 " AND String(pafr_fecemb,'dd-mm-yyyy')	=	'" + String(ld_fecemb, "dd-mm-yyyy") + "'" + &
							 " AND pafr_calibr = '" + ls_calibr + "'", 1, dw_1.RowCount())

//							 " AND String(pafr_fecemb,'dd-mm-yyyy')	=	'" + String(ls_fecemb, "dd-mm-yyyy") + "'" + &


IF ll_fila > 0 and ll_fila <> il_fila THEN
	MessageBox("Error","Calidad ya fue ingresada anteriormente",Information!, Ok!)
	RETURN True
ELSE
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
		FROM  dbo.productores
		WHERE prod_codigo = :ll_codigo ;
	
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

public function boolean varificaproductor (long ll_productor);Long		ll_cont
Integer 	li_cliente,li_planta
 
li_cliente	=	Integer(istr_mant.argumento[1])
li_planta	=	Integer(istr_mant.argumento[21])

SELECT Sum(plde_codigo)
	INTO :ll_cont  
	FROM dbo.prodpacking  
	WHERE prod_codigo = :ll_productor AND  
         plde_codigo = :li_planta;
			
IF (sqlca.sqlcode)= -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla Prodpacking")
	RETURN FALSE
ELSEIF ll_cont>0 THEN
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
FROM dbo.spro_prodpredio
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

public function boolean noexistepacking (integer ai_packing);Integer	li_Existe

SELECT	Count(*)
	INTO	:li_Existe 
   FROM	dbo.plantadesp  
   WHERE plde_codigo	=	:ai_Packing
	AND	plde_tipopl	=	2 ;
	
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura Tabla PLANTADESP")
	RETURN True
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención", "Codigo Packing no Existe. Ingrese otro")
	RETURN True
ELSEIF li_Existe > 0 THEN
	RETURN False
ELSE 
	MessageBox("Atención", "Codigo Packing no Existe. Ingrese otro")
	RETURN True
END IF

RETURN False
end function

public function boolean noexisteproductorrotulado (string ls_columna);String	ls_nombre
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
	FROM    dbo.productores
	WHERE	   prod_codigo = :ll_codigo ;
	
	IF sqlca.SQLCode = -1 THEN
		F_errorbasedatos(sqlca,"Lectura tabla Productores")
		Return lb_Retorna
		
	ELSEIF sqlca.SQLCode = 100 THEN
		MessageBox("Atención", "Código de Productor no ha sido creado, Ingrese otro Código.", &
		Exclamation!, OK!)
		Return lb_Retorna
	ELSE
		//istr_mant.argumento[13]	=	string(ll_codigo)
		dw_1.SetItem (il_fila, "pafr_prdrot", ll_codigo)
		//dw_1.SetItem (il_fila, "productores_prod_nombre", ls_nombre)
		lb_retorna = False
		RETURN lb_retorna
	END IF

Return lb_retorna
end function

public function boolean noexistecuartel (long al_productor, integer ai_predio, integer ai_cuartel, integer ai_especie, integer ai_variedad);Integer li_existe

SELECT Count(*) 
INTO :li_existe
FROM dbo.spro_prodcuarteles
WHERE prod_codigo = :al_productor
AND	prpr_codigo = :ai_predio
AND 	prcc_codigo = :ai_cuartel
AND	espe_codigo = :ai_especie
AND 	vari_codigo = :ai_variedad;

IF (sqlca.sqlcode)= -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla spro_prodcuarteles")
	RETURN FALSE
ELSEIF li_existe>0 THEN
	RETURN TRUE
ELSE
	MessageBox("Atención", "Codigo de Cuartel no Existe.", &
					Exclamation!, OK!)
	RETURN FALSE
END IF
end function

public function boolean noexistecategoria (integer categoria);Boolean	lb_retorna = True
Long		ll_cont


	SELECT	count(*) INTO :ll_cont
		FROM  dbo.categorias
		WHERE cate_codigo = :categoria ;
	
	IF sqlca.SQLCode = -1 THEN
		F_errorbasedatos(sqlca,"Lectura Tabla Categorias")
		Return lb_Retorna
		
	ELSEIF ll_cont = 0 THEN
		MessageBox("Atención", "Código de Categoria no ha sido creado, Ingrese otro Código.", &
		Exclamation!, OK!)
		Return lb_Retorna
	ELSE
		lb_retorna = False
		RETURN lb_retorna
	END IF

Return lb_retorna
end function

on w_mant_deta_palletfruta_interfecemb_rotulatrans.create
call super::create
end on

on w_mant_deta_palletfruta_interfecemb_rotulatrans.destroy
call super::destroy
end on

event ue_recuperadatos;call super::ue_recuperadatos;ias_campo[13]	=	String(dw_1.Object.prod_codigo[il_fila])
ias_campo[12]	=	dw_1.Object.pafr_calibr[il_fila]
ias_campo[11]	=	String(dw_1.Object.pafr_ccajas[il_fila])
ias_campo[14]	=	String(dw_1.Object.pafr_nrlote[il_fila])

IF istr_mant.agrega THEN
	dw_1.SetItem(il_fila, "prod_codigo", dw_1.GetItemNumber(dw_1.GetRow(),'prod_codigo'))
	dw_1.SetItem(il_fila, "clie_codigo", Integer(istr_mant.argumento[1]))
	dw_1.SetItem(il_fila, "paen_numero", Long(istr_mant.argumento[2]))
	dw_1.SetItem(il_fila, "espe_codigo", Integer(istr_mant.argumento[3]))
	dw_1.SetItem(il_fila, "vari_codigo", Integer(istr_mant.argumento[4]))
	dw_1.SetItem(il_fila, "pafr_varrot", Integer(istr_mant.argumento[4]))
	dw_1.SetItem(il_fila, "variedades_vari_nombre", istr_mant.argumento[5])
	dw_1.SetItem(il_fila, "vari_codigo", Integer(istr_mant.argumento[4]))
	dw_1.SetItem(il_fila, "variedades_vari_nombre", istr_mant.argumento[5])
	dw_1.SetItem(il_fila, "emba_codigo", istr_mant.argumento[7])
	dw_1.SetItem(il_fila, "pafr_embrea", istr_mant.argumento[7])
	dw_1.SetItem(il_fila, "embalajesprod_emba_nombre", istr_mant.argumento[8])
	dw_1.SetItem(il_fila, "cond_codigo", Integer(istr_mant.argumento[10]))
	dw_1.SetItem(il_fila, "etiq_codigo", Integer(istr_mant.argumento[9]))
	dw_1.SetItem(il_fila, "plde_codigo", Integer(istr_mant.argumento[6]))
	dw_1.SetItem(il_fila, "pafr_fecemb", Date(istr_mant.argumento[40]))
	dw_1.SetItem(il_fila, "pafr_fecrot", Date(istr_mant.argumento[40]))
	dw_1.SetItem(il_fila, "cate_codigo", Integer(istr_mant.argumento[55]))
	dw_1.SetItem(il_fila, "pafr_catrot", Integer(istr_mant.argumento[55]))
	istr_mant.argumento[65] = istr_mant.argumento[55]
	//dw_1.SetItem(il_fila, "pafr_copack", Integer(istr_mant.argumento[21]))
	
	//dw_1.SetItem(il_fila, "pafr_rotpak", dw_1.Object.pafr_copack[il_fila])
	//dw_1.SetItem(il_fila, "pafr_huert4", dw_1.Object.pafr_huert1[il_fila])
	//dw_1.SetItem(il_fila, "pafr_cuart4", dw_1.Object.pafr_cuart1[il_fila])
	
END IF

IF istr_mant.agrega = False and istr_mant.borra = False THEN
	istr_mant.argumento[11]=String(Long(istr_mant.argumento[11])+Long(ias_campo[11]))
	dw_1.SetItem(il_fila, "pafr_fecemb", Date(istr_mant.argumento[40]))
	//dw_1.SetItem(il_fila, "pafr_copack", Integer(istr_mant.argumento[21]))
ELSE
	dw_1.SetItem(il_fila, "pafr_ccajas", Integer(istr_mant.argumento[11]))
	dw_1.SetItem(il_fila, "pafr_fecemb", Date(istr_mant.argumento[40]))
	//dw_1.SetItem(il_fila, "pafr_copack", Integer(istr_mant.argumento[21]))
END IF


end event

event ue_deshace;call super::ue_deshace;dw_1.Object.prod_codigo[il_fila]	=	Long(ias_campo[13])
dw_1.Object.pafr_calibr[il_fila]	=	ias_campo[12]
dw_1.Object.pafr_ccajas[il_fila]	=	Integer(ias_campo[11])
dw_1.Object.pafr_nrlote[il_fila]	=	Integer(ias_campo[14])
end event

event ue_antesguardar;call super::ue_antesguardar;Integer	li_cont
String	ls_mensaje, ls_colu[]

IF Isnull(dw_1.Object.prod_codigo[il_fila]) OR dw_1.Object.prod_codigo[il_fila] = 0 THEN
	li_cont ++
	ls_mensaje 			= ls_mensaje + "~nCódigo de Productor"
	ls_colu[li_cont]	= "prod_codigo"
END IF

IF Isnull(dw_1.Object.pafr_prdrot[il_fila]) OR dw_1.Object.pafr_prdrot[il_fila] = 0 THEN
	li_cont ++
	ls_mensaje 			= ls_mensaje + "~nCódigo de Productor Rotulado"
	ls_colu[li_cont]	= "pafr_prdrot"
END IF

IF Isnull(dw_1.Object.pafr_calibr[il_fila]) OR dw_1.Object.pafr_calibr[il_fila] ="" THEN
	li_cont ++
	ls_mensaje			= ls_mensaje + "~nCalibre del Pallet"
	ls_colu[li_cont]	= "pafr_calibr"
END IF

IF Isnull(dw_1.Object.pafr_calrot[il_fila]) OR dw_1.Object.pafr_calrot[il_fila] ="" THEN
	li_cont ++
	ls_mensaje			= ls_mensaje + "~nCalibre del Pallet Rotulado"
	ls_colu[li_cont]	= "pafr_calrot"
END IF

IF Isnull(dw_1.Object.emba_codigo[il_fila]) OR dw_1.Object.emba_codigo[il_fila] ="" THEN
	li_cont ++
	ls_mensaje			= ls_mensaje + "~nEmbalaje Rotulado"
	ls_colu[li_cont]	= "emba_codigo"
END IF

IF Isnull(dw_1.Object.pafr_embrea[il_fila]) OR dw_1.Object.pafr_embrea[il_fila] ="" THEN
	li_cont ++
	ls_mensaje			= ls_mensaje + "~nEmbalaje Real "
	ls_colu[li_cont]	= "pafr_embrea"
END IF

IF Isnull(dw_1.Object.pafr_huert1[il_fila]) OR dw_1.Object.pafr_huert1[il_fila] = 0 THEN
	li_cont ++
	ls_mensaje			= ls_mensaje + "~nHuerto"
	ls_colu[li_cont]	= "pafr_huert1"
END IF

IF Isnull(dw_1.Object.pafr_huert4[il_fila]) OR dw_1.Object.pafr_huert4[il_fila] = 0 THEN
	li_cont ++
	ls_mensaje			= ls_mensaje + "~nHuerto Rotulado"
	ls_colu[li_cont]	= "pafr_huert4"
END IF

IF Isnull(dw_1.Object.pafr_cuart1[il_fila]) OR dw_1.Object.pafr_cuart1[il_fila] = 0 THEN
	li_cont ++
	ls_mensaje			= ls_mensaje + "~nCuartel"
	ls_colu[li_cont]	= "pafr_cuart1"
END IF

IF Isnull(dw_1.Object.pafr_cuart4[il_fila]) OR dw_1.Object.pafr_cuart4[il_fila] = 0 THEN
	li_cont ++
	ls_mensaje			= ls_mensaje + "~nCuartel Rotulado"
	ls_colu[li_cont]	= "pafr_cuart4"
END IF

IF Isnull(dw_1.Object.pafr_ccajas[il_fila]) OR dw_1.Object.pafr_ccajas[il_fila] = 0 THEN
	li_cont ++
	ls_mensaje 			= ls_mensaje + "~nCantidad de Cajas"
	ls_colu[li_cont]	= "pafr_ccajas"
END IF

IF Isnull(dw_1.Object.pafr_copack[il_fila]) OR dw_1.Object.pafr_copack[il_fila] = 0 THEN
	li_cont ++
	ls_mensaje 			= ls_mensaje + "~nCódigo de Packing"
	ls_colu[li_cont]	= "pafr_copack"
END IF
//IF Isnull(dw_1.Object.pafr_nrlote[il_fila]) OR dw_1.Object.pafr_nrlote[il_fila] = 0 THEN
//	li_cont ++
//	ls_mensaje 			= ls_mensaje + "~nNúmero de Lote"
//	ls_colu[li_cont]	= "pafr_nrlote"
//END IF

IF li_cont > 0 THEN
	MessageBox("Error de Consistencia", "Falta el ingreso de : " + ls_mensaje + ".", StopSign!, Ok!)
	dw_1.SetColumn(ls_colu[1])
	dw_1.SetFocus()
	Message.DoubleParm = -1
ELSE
	istr_mant.Argumento[11]=String(Long(istr_mant.Argumento[11])-Long(dw_1.Object.pafr_ccajas[il_fila]))
END IF
end event

event ue_nuevo;call super::ue_nuevo;
IF Long(istr_mant.argumento[11])<1 THEN
	MessageBox("Atención","Se ha completado las cajas del Pallet.")
	dw_1.Enabled =  False
	pb_acepta.SetFocus()
//	istr_mant.respuesta = 0
ELSE
	dw_1.SetItem(il_fila, "clie_codigo", Integer(istr_mant.argumento[1]))
	dw_1.SetItem(il_fila, "paen_numero", Long(istr_mant.argumento[2]))
	dw_1.SetItem(il_fila, "espe_codigo", Integer(istr_mant.argumento[3]))
	dw_1.SetItem(il_fila, "vari_codigo", Integer(istr_mant.argumento[4]))
	dw_1.SetItem(il_fila, "pafr_varrot", Integer(istr_mant.argumento[4]))
	dw_1.SetItem(il_fila, "variedades_vari_nombre", istr_mant.argumento[5])
	dw_1.SetItem(il_fila, "emba_codigo", istr_mant.argumento[59])
	dw_1.SetItem(il_fila, "pafr_embrea", istr_mant.argumento[7])
	dw_1.SetItem(il_fila, "embalajesprod_emba_nombre", istr_mant.argumento[8])
	dw_1.SetItem(il_fila, "cond_codigo", Integer(istr_mant.argumento[10]))
	dw_1.SetItem(il_fila, "etiq_codigo", Integer(istr_mant.argumento[9]))
	dw_1.SetItem(il_fila, "plde_codigo", Integer(istr_mant.argumento[6]))
	dw_1.SetItem(il_fila, "pafr_fecemb", Date(istr_mant.argumento[28]))
	dw_1.SetItem(il_fila, "pafr_fecrot", Date(istr_mant.argumento[60]))
	dw_1.SetItem(il_fila, "pafr_fecing", Date(istr_mant.argumento[39]))
	dw_1.SetItem(il_fila, "prod_codigo", Long(istr_mant.argumento[50]))
	dw_1.SetItem(il_fila, "pafr_prdrot", Long(istr_mant.argumento[51]))
	dw_1.SetItem(il_fila, "pafr_huert1", Integer(istr_mant.argumento[52]))
	dw_1.SetItem(il_fila, "pafr_huert4", Integer(istr_mant.argumento[53]))
	dw_1.SetItem(il_fila, "pafr_cuart1", Integer(istr_mant.argumento[54]))
	dw_1.SetItem(il_fila, "pafr_cuart4", Integer(istr_mant.argumento[55]))
	dw_1.SetItem(il_fila, "pafr_calibr", istr_mant.argumento[56])
	dw_1.SetItem(il_fila, "pafr_calrot", istr_mant.argumento[57])
	dw_1.SetItem(il_fila, "pafr_ccajas", Long(istr_mant.Argumento[11]))
	dw_1.SetItem(il_fila, "cate_codigo", Integer(istr_mant.argumento[65]))
	dw_1.SetItem(il_fila, "pafr_catrot", Integer(istr_mant.argumento[65]))
	
	
	//dw_1.SetItem(il_fila, "pafr_rotpak", dw_1.Object.pafr_copack[il_fila])
	//dw_1.SetItem(il_fila, "pafr_huert4", dw_1.Object.pafr_huert1[il_fila])
	//dw_1.SetItem(il_fila, "pafr_cuart4", dw_1.Object.pafr_cuart1[il_fila])
	dw_1.SetColumn("prod_codigo")
	
END IF



end event

event open;x	= 100
y	= 450

PostEvent("ue_recuperadatos")

istr_mant = Message.PowerObjectParm

IF istr_mant.argumento[16]='1' THEN
	dw_1.Enabled	=	False
END IF

IF istr_mant.solo_consulta = True THEN
	dw_1.Enabled	=	False
END IF

dw_1.GetChild("espe_codigo", dw_especies)
dw_1.GetChild("vari_codigo", dw_variedades)
dw_1.GetChild("pafr_varrot", idwc_valrot)
dw_1.GetChild("emba_codigo", dw_embalajes)
dw_1.GetChild("pafr_embrea", idwc_embarot)
dw_1.GetChild("etiq_codigo", dw_etiquetas)
dw_1.GetChild("plde_codigo", dw_plantadesp)
dw_1.GetChild("prod_codigo", dw_productor)
dw_1.GetChild("pafr_calibr", dw_calibres)
dw_1.GetChild("pafr_calrot", idwc_calrot)
dw_1.GetChild("pafr_prdrot", idwc_prodrot)
dw_1.GetChild("pafr_copack", idw_ptaori)
dw_1.GetChild("cate_codigo", idwc_categoria)
dw_1.GetChild("pafr_catrot", idwc_caterotula)

dw_especies.SetTransObject(sqlca)
dw_variedades.SetTransObject(sqlca)
idwc_valrot.SetTransObject(sqlca)
dw_embalajes.SetTransObject(sqlca)
dw_etiquetas.SetTransObject(sqlca)
dw_plantadesp.SetTransObject(sqlca)
dw_productor.SetTransObject(sqlca)
dw_calibres.SetTransObject(sqlca)
idwc_calrot.SetTransObject(sqlca)
idwc_prodrot.SetTransObject(sqlca)
idwc_embarot.SetTransObject(sqlca)
idwc_caterotula.SetTransObject(sqlca)
idwc_categoria.SetTransObject(sqlca)
idw_ptaori.SetTransObject(sqlca)

idwc_caterotula.Retrieve()
idwc_categoria.Retrieve()
dw_especies.Retrieve()
dw_variedades.Retrieve(Integer(istr_mant.argumento[3]))
idwc_valrot.Retrieve(Integer(istr_mant.argumento[3]))					  							  
dw_embalajes.Retrieve(Integer(istr_mant.Argumento[1]))
idwc_embarot.Retrieve(Integer(istr_mant.Argumento[1]))
dw_etiquetas.Retrieve()
dw_plantadesp.Retrieve(Integer(istr_mant.Argumento[6]))
dw_productor.Retrieve(Integer(istr_mant.Argumento[1]))
idwc_prodrot.Retrieve(Integer(istr_mant.Argumento[1]))
dw_calibres.Retrieve(Integer(istr_mant.Argumento[3]), &
							Integer(istr_mant.Argumento[4]))
idwc_calrot.Retrieve(Integer(istr_mant.Argumento[3]), &
							Integer(istr_mant.Argumento[4]))				

idw_ptaori.Retrieve(gi_codplanta)
idw_ptaori.Setfilter("plde_tipopl=2")
idw_ptaori.Filter()

idw_ptaori.SetSort("plde_nombre")
idw_ptaori.Sort( )

dw_1.SetTransObject(sqlca)
istr_mant.dw.ShareData(dw_1)


end event

type pb_ultimo from w_mant_detalle_csd`pb_ultimo within w_mant_deta_palletfruta_interfecemb_rotulatrans
end type

type pb_siguiente from w_mant_detalle_csd`pb_siguiente within w_mant_deta_palletfruta_interfecemb_rotulatrans
end type

type pb_anterior from w_mant_detalle_csd`pb_anterior within w_mant_deta_palletfruta_interfecemb_rotulatrans
end type

type pb_primero from w_mant_detalle_csd`pb_primero within w_mant_deta_palletfruta_interfecemb_rotulatrans
end type

type pb_cancela from w_mant_detalle_csd`pb_cancela within w_mant_deta_palletfruta_interfecemb_rotulatrans
integer x = 3557
integer y = 524
end type

type pb_acepta from w_mant_detalle_csd`pb_acepta within w_mant_deta_palletfruta_interfecemb_rotulatrans
integer x = 3557
integer y = 312
end type

event pb_acepta::clicked;istr_mant.respuesta = 1

IF istr_mant.agrega THEN
	
	istr_mant.argumento[59] = dw_1.Object.emba_codigo[il_fila]
	istr_mant.argumento[58] = String(dw_1.Object.pafr_varrot[il_fila])
	istr_mant.argumento[60] = String(dw_1.Object.pafr_fecrot[il_fila])
	
	Parent.TriggerEvent("ue_nuevo")

//	IF istr_mant.respuesta = 0 THEN
//		pb_salir.TriggerEvent(clicked!)
//	END IF
ELSE
	CloseWithReturn(Parent, istr_mant)
END IF
end event

type pb_salir from w_mant_detalle_csd`pb_salir within w_mant_deta_palletfruta_interfecemb_rotulatrans
integer x = 3557
integer y = 744
end type

type dw_1 from w_mant_detalle_csd`dw_1 within w_mant_deta_palletfruta_interfecemb_rotulatrans
integer x = 73
integer y = 112
integer width = 3296
integer height = 1500
string dataobject = "dw_mant_deta_palletfruta_interfecemb_rotulatrans"
end type

event dw_1::itemchanged;String	ls_columna, ls_nula
Integer  li_lote
Long		ll_Prod
SetNull(ls_Nula)

ls_columna = dwo.name

Choose Case ls_columna	
	Case "pafr_embrea"
		istr_mant.Argumento[7]=Data
		
		If IsNull(dw_1.Object.emba_codigo[il_fila]) Then
			dw_1.SetItem(il_fila, "emba_codigo", data)
		End If	
	
	Case "emba_codigo"
		istr_mant.Argumento[59]=Data

	Case "vari_codigo"
		istr_mant.argumento[58] = String(existevariedad_relacionada(Integer(istr_mant.argumento[3]),integer(data)))
		
		istr_mant.Argumento[4]=Data
		dw_1.SetItem(il_fila, "pafr_cuart1", Long(ls_Nula))
		dw_1.SetItem(il_fila, "pafr_calibr", ls_Nula)
		
		dw_1.Object.pafr_varrot[il_fila] = Integer(istr_mant.argumento[58])
		
		If IsNull(dw_1.Object.pafr_varrot[il_fila]) OR IsNull(Integer(istr_mant.argumento[58])) Then
			dw_1.SetItem(il_fila, "pafr_varrot", Integer(data))
			istr_mant.argumento[58] = data
		End If	
		
		If IsNull(dw_1.Object.pafr_cuart4[il_fila]) Then
			dw_1.SetItem(il_fila, "pafr_cuart4", Long(ls_Nula))
		End If
		
		If IsNull(dw_1.Object.pafr_calrot[il_fila]) Then
			dw_1.SetItem(il_fila, "pafr_calrot", ls_Nula)
		End If
		
	Case "pafr_varrot"
		dw_1.SetItem(il_fila, "pafr_cuart4", Long(ls_Nula))
		dw_1.SetItem(il_fila, "pafr_calrot", ls_Nula)
		istr_mant.argumento[58] = data
			
	Case "prod_codigo"
		dw_1.SetItem(il_fila, "pafr_cuart1", Integer(ls_Nula))
		dw_1.SetItem(il_fila, "pafr_huert1", Integer(ls_Nula))
		
		If IsNull(dw_1.Object.pafr_cuart4[il_fila]) Then
			dw_1.SetItem(il_fila, "pafr_cuart4", Integer(ls_Nula))
		End If
		
		If IsNull(dw_1.Object.pafr_huert4[il_fila]) Then
			dw_1.SetItem(il_fila, "pafr_huert4", Integer(ls_Nula))
		End If
		
		If  NoExisteProductor(data) Then
			dw_1.SetItem(il_fila, ls_columna, Long(ls_Nula))
			Return 1
		ElseIf istr_mant.Argumento[20]='1' OR istr_mant.Argumento[20]='6' OR istr_mant.Argumento[20]='9' Then
			istr_mant.argumento[50] = data
			
			dw_1.GetChild("pafr_huert1", idwc_predio)
			idwc_predio.SetTransObject(SQLCA)
			ll_Prod	=	Long(data)
			
			dw_1.SetItem(il_fila, "pafr_huert1", Long(ls_Nula))
			If idwc_predio.Retrieve(ll_Prod) = 0 Then
				idwc_predio.InsertRow(0)
			End If
			
			If IsNull(dw_1.Object.pafr_prdrot[il_fila]) Then
				dw_1.SetItem(il_fila, "pafr_prdrot", Long(data))
				istr_mant.argumento[51] = data
			End If
			
			If IsNull(dw_1.Object.pafr_huert4[il_fila]) Then
				dw_1.GetChild("pafr_huert4", idwc_prediorot)
				idwc_prediorot.SetTransObject(SQLCA)
				If idwc_prediorot.Retrieve(ll_Prod) = 0 Then
					idwc_prediorot.InsertRow(0)
				End If
			End If
			
			This.Object.pafr_ggncod[Row] = f_AsignaGGN(Long(Data), This.Object.pafr_huert1[Row], This.Object.espe_codigo[Row], This.Object.pafr_fecemb[Row], True)
			
			If VarIficaproductor(Integer(Data))=FALSE Then
				Return 1
			End If
		End If
				
	Case "pafr_huert1"
		dw_1.SetItem(il_fila, "pafr_cuart1", Integer(ls_Nula))
		
		If Not noexistepredio(dw_1.Object.Prod_codigo[row],integer(data)) Then
			dw_1.SetItem(row, "pafr_huert1", integer(ls_nula))
			Return 1
		Else
			
			istr_mant.argumento[52] = data
					
			dw_1.GetChild("pafr_cuart1", idwc_cuartel)
			idwc_cuartel.SetTransObject(SQLCA)
			idwc_cuartel.Retrieve(dw_1.Object.Prod_codigo[row],integer(data),dw_1.Object.espe_codigo[row],dw_1.Object.vari_codigo[row])
			
			If IsNull(dw_1.Object.pafr_huert4[il_fila]) Then
				dw_1.SetItem(il_fila, "pafr_huert4", Integer(data))
				istr_mant.argumento[53] = data
			End If
			
			If IsNull(dw_1.Object.pafr_cuart4[il_fila]) Then
				dw_1.GetChild("pafr_cuart4", idwc_cuartelrot)
				idwc_cuartelrot.SetTransObject(SQLCA)
				idwc_cuartelrot.Retrieve(dw_1.Object.Prod_codigo[row],integer(data),dw_1.Object.espe_codigo[row],dw_1.Object.vari_codigo[row])
				dw_1.SetItem(il_fila, "pafr_cuart4", Integer(ls_nula))
			End If
		End If
		
		This.Object.pafr_ggncod[Row] = f_AsignaGGN(This.Object.prod_codigo[Row], Integer(Data), This.Object.espe_codigo[Row], This.Object.pafr_fecemb[Row], True)
		
	Case "pafr_calibr"
		If NoExisteCalibre(data) OR Duplicado(Upper(data)) Then
			dw_1.SetItem(il_fila, ls_columna, Upper(ias_campo[12]))
			Return 1
		End If
		
		istr_mant.argumento[56] = data
		
		If IsNull(dw_1.Object.pafr_calrot[il_fila]) Then
			istr_mant.argumento[57] = data
			dw_1.SetItem(il_fila, "pafr_calrot", data)
		End If
		
	Case "pafr_ccajas"
		If Long(data) < 0 Then
			dw_1.SetItem(il_fila, ls_columna, Long(istr_mant.Argumento[11]))
			Return 1
		End If
		
		If Long(data) > Long(istr_mant.Argumento[11]) Then
			MessageBox("Atención", "Cajas ingresadas sobrepasan las " + istr_mant.Argumento[11] + &
							" cajas del Pallet")
			dw_1.SetItem(il_fila, ls_columna, Long(istr_mant.Argumento[11]))
			Return 1
		End If
		
	Case "pafr_prdrot"
		dw_1.SetItem(il_fila, "pafr_cuart4", Integer(ls_Nula))
		dw_1.SetItem(il_fila, "pafr_huert4", Integer(ls_Nula))
		
		If  noexisteproductorrotulado(data) Then
			dw_1.SetItem(il_fila, ls_columna, Long(ls_Nula))
			Return 1
		ElseIf istr_mant.Argumento[20]='1' OR istr_mant.Argumento[20]='6' Then
			istr_mant.argumento[51] = data

			dw_1.GetChild("pafr_huert4", idwc_prediorot)
			idwc_prediorot.SetTransObject(SQLCA)
			ll_Prod	=	Long(data)
			If idwc_prediorot.Retrieve(ll_Prod) = 0 Then
				idwc_prediorot.InsertRow(0)
			End If
			
			If VarIficaproductor(Integer(Data))=FALSE Then
				Return 1
			End If
		End If	
		
		This.Object.pafr_ggncod[Row] = f_AsignaGGN(Long(Data), This.Object.pafr_huert4[Row], This.Object.espe_codigo[Row], This.Object.pafr_fecemb[Row], True)
		
	Case "pafr_huert4"
		dw_1.SetItem(il_fila, "pafr_cuart4", Integer(ls_Nula))
		If Not noexistepredio(dw_1.Object.pafr_prdrot[row],integer(data)) Then
			dw_1.SetItem(row, "pafr_huert4", integer(ls_nula))
			Return 1
		Else
			dw_1.GetChild("pafr_cuart4", idwc_cuartelrot)
			idwc_cuartelrot.SetTransObject(SQLCA)
			idwc_cuartelrot.Retrieve(dw_1.Object.pafr_prdrot[row],integer(data),dw_1.Object.espe_codigo[row],dw_1.Object.pafr_varrot[row])
			This.Object.pafr_ggncod[Row] = f_AsignaGGN(This.Object.pafr_prdrot[Row], Integer(Data), This.Object.espe_codigo[Row], This.Object.pafr_fecemb[Row], True)
		End If
		
		istr_mant.argumento[53] = data
		
	Case "pafr_cuart1"	
		If Not noexistecuartel(dw_1.Object.Prod_codigo[row],dw_1.Object.pafr_huert1[row],integer(data),dw_1.Object.espe_codigo[row],dw_1.Object.vari_codigo[row]) Then
			dw_1.SetItem(row, "pafr_cuart1", integer(ls_nula))
			Return 1
		End If
		
		If IsNull(dw_1.Object.pafr_calrot[il_fila]) Then
			dw_1.SetItem(row, "pafr_cuart4", integer(data))
			istr_mant.argumento[55] = data
		End If		
		
		istr_mant.argumento[54] = data
			
	Case "pafr_cuart4"
		If Not noexistecuartel(dw_1.Object.pafr_prdrot[row],dw_1.Object.pafr_huert4[row],integer(data),dw_1.Object.espe_codigo[row],dw_1.Object.pafr_varrot[row]) Then
			dw_1.SetItem(row, "pafr_cuart4", integer(ls_nula))
			Return 1
		End If
		
		istr_mant.argumento[55] = data	
		
	Case "pafr_copack"	
		If NoExistePacking(Integer(Data)) Then
			dw_1.SetItem(il_fila, ls_columna, Integer(ls_Nula))
			Return 1
		Else	
			dw_1.Object.pafr_rotpak[il_fila] = dw_1.Object.pafr_copack[il_fila]
		End If
		
		dw_1.SetItem(il_fila, "pafr_rotpak", integer(data))	
	
	Case "pafr_fecemb"	
		If IsNull(dw_1.Object.pafr_fecrot[il_fila]) Then
			dw_1.SetItem(row, "pafr_fecrot", Date(data))
			istr_mant.argumento[60] = data
		End If
		
	Case "pafr_calrot"	
		istr_mant.argumento[57] = data
		
	Case "pafr_fecrot"	
		istr_mant.argumento[60] = data	
		
	Case "cate_codigo"	
		If noexistecategoria(Integer(Data)) Then
			dw_1.SetItem(il_fila, ls_columna, Integer(ls_Nula))
			dw_1.SetItem(il_fila, "pafr_catrot", Integer(ls_Nula))
			Return 1
		Else	
			dw_1.SetItem(row, "pafr_catrot", Integer(data))
			istr_mant.argumento[65] = data
		End If
		
	Case "pafr_catrot"	
		If noexistecategoria(Integer(Data)) Then
			dw_1.SetItem(il_fila, ls_columna, Integer(ls_Nula))
			Return 1
		End If	
		
End Choose
	

end event

event dw_1::clicked;call super::clicked;String	ls_columna

ls_columna = dwo.name

CHOOSE CASE ls_columna
	CASE "buscaprod"
		buscaprod()
	
END CHOOSE
end event

