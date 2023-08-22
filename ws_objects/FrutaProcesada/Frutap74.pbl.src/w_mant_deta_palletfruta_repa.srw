$PBExportHeader$w_mant_deta_palletfruta_repa.srw
forward
global type w_mant_deta_palletfruta_repa from w_mant_detalle_csd
end type
end forward

global type w_mant_deta_palletfruta_repa from w_mant_detalle_csd
integer width = 3232
integer height = 1576
string title = "Detalle Pallet"
end type
global w_mant_deta_palletfruta_repa w_mant_deta_palletfruta_repa

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
public function boolean duplicado (string campo)
public function boolean noexisteproductor (string ls_columna)
public subroutine buscaprod ()
public function boolean varificaproductor (long ll_productor)
public function boolean noexistepredio (long ai_productor, integer ai_predio)
public function boolean noexistefechaingreso (string ls_columna)
public function boolean noexistefechaembalaje (string ls_columna)
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

public function boolean duplicado (string campo);Long		ll_fila
String	ls_client, ls_nropal, ls_especi, ls_varied, ls_embala, ls_produc, ls_condic, ls_etique, &
			ls_planta, ls_zonaco, ls_calibr, ls_huert1, ls_cuart1

	
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
							 " AND pafr_calibr = '" + ls_calibr + "'", 1, dw_1.RowCount())

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

//IF istr_mant.argumento[17] = '1'and istr_mant.argumento[24] <> '1' THEN
//	IF Pos(istr_mant.argumento[15], "," + String(ll_codigo)) = 0 THEN
//		messagebox("Atención","Productor no corresponde a Pallets originales", Exclamation!, Ok!)
//		RETURN lb_Retorna
//	END IF
//END IF

IF istr_mant.argumento[17] = '1'and istr_mant.argumento[24] <> '1' THEN
	IF Pos(istr_mant.argumento[47], "," + String(ll_codigo)) = 0 THEN
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

public function boolean noexistefechaingreso (string ls_columna);String	ls_nombre
Date ld_pafr_fecing
Boolean	lb_retorna =False

ld_pafr_fecing = Date(ls_columna)

IF istr_mant.argumento[45]<> ''  AND  istr_mant.argumento[45]<> ',' THEN
	IF Pos(istr_mant.argumento[45], "," + String(ld_pafr_fecing)) = 0 THEN
		messagebox("Atención","Fecha de Ingreso no corresponde a Pallets originales", Exclamation!, Ok!)
		 lb_Retorna	= True
	END IF
END IF
 
RETURN lb_Retorna 

end function

public function boolean noexistefechaembalaje (string ls_columna);String	ls_nombre
Date ld_pafr_fecemb
Boolean	lb_retorna =False

ld_pafr_fecemb = Date(ls_columna)

IF istr_mant.argumento[46]<> '' AND  istr_mant.argumento[46]<> ',' THEN
	IF Pos(istr_mant.argumento[46], "," + String(ld_pafr_fecemb)) = 0 THEN
		messagebox("Atención","Fecha de Embalaje no corresponde a Pallets originales", Exclamation!, Ok!)
		 lb_Retorna	= True
	END IF
END IF

RETURN lb_Retorna  

end function

on w_mant_deta_palletfruta_repa.create
call super::create
end on

on w_mant_deta_palletfruta_repa.destroy
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
	dw_1.SetItem(il_fila, "variedades_vari_nombre", istr_mant.argumento[5])
	dw_1.SetItem(il_fila, "vari_codigo", Integer(istr_mant.argumento[4]))
	dw_1.SetItem(il_fila, "variedades_vari_nombre", istr_mant.argumento[5])
	dw_1.SetItem(il_fila, "emba_codigo", istr_mant.argumento[7])
	dw_1.SetItem(il_fila, "embalajesprod_emba_nombre", istr_mant.argumento[8])
	dw_1.SetItem(il_fila, "cond_codigo", Integer(istr_mant.argumento[10]))
	dw_1.SetItem(il_fila, "etiq_codigo", Integer(istr_mant.argumento[9]))
	dw_1.SetItem(il_fila, "plde_codigo", Integer(istr_mant.argumento[6]))
	//dw_1.SetItem(il_fila, "pafr_fecemb", Date(istr_mant.argumento[40]))
END IF

IF istr_mant.agrega = False and istr_mant.borra = False THEN
	istr_mant.argumento[11]=String(Long(istr_mant.argumento[11])+Long(ias_campo[11]))
ELSE
	dw_1.SetItem(il_fila, "pafr_ccajas", Integer(istr_mant.argumento[11]))
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

IF Isnull(dw_1.Object.pafr_calibr[il_fila]) OR dw_1.Object.pafr_calibr[il_fila] ="" THEN
	li_cont ++
	ls_mensaje			= ls_mensaje + "~nCalibre del Pallet"
	ls_colu[li_cont]	= "pafr_calibr"
END IF

IF Isnull(dw_1.Object.pafr_ccajas[il_fila]) OR dw_1.Object.pafr_ccajas[il_fila] = 0 THEN
	li_cont ++
	ls_mensaje 			= ls_mensaje + "~nCantidad de Cajas"
	ls_colu[li_cont]	= "pafr_ccajas"
END IF

IF Isnull(dw_1.Object.pafr_fecing[il_fila]) OR dw_1.Object.pafr_fecing[il_fila] =  Date('1900-01-01') THEN
	li_cont ++
	ls_mensaje 			= ls_mensaje + "~nFecha de Ingreso"
	ls_colu[li_cont]	= "pafr_fecing"
END IF

IF Isnull(dw_1.Object.pafr_fecemb[il_fila]) OR dw_1.Object.pafr_fecemb[il_fila] =  Date('1900-01-01') THEN
	li_cont ++
	ls_mensaje 			= ls_mensaje + "~nFecha de Embalaje"
	ls_colu[li_cont]	= "pafr_fecemb"
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
	dw_1.SetItem(il_fila, "variedades_vari_nombre", istr_mant.argumento[5])
	dw_1.SetItem(il_fila, "emba_codigo", istr_mant.argumento[7])
	dw_1.SetItem(il_fila, "embalajesprod_emba_nombre", istr_mant.argumento[8])
	dw_1.SetItem(il_fila, "cond_codigo", Integer(istr_mant.argumento[10]))
	dw_1.SetItem(il_fila, "etiq_codigo", Integer(istr_mant.argumento[9]))
	dw_1.SetItem(il_fila, "plde_codigo", Integer(istr_mant.argumento[6]))
	
	dw_1.SetItem(il_fila, "pafr_rotpak", dw_1.Object.pafr_copack[il_fila])
	dw_1.SetItem(il_fila, "pafr_huert4", dw_1.Object.pafr_huert1[il_fila])
	dw_1.SetItem(il_fila, "pafr_cuart4", dw_1.Object.pafr_cuart1[il_fila])
	
	
	dw_1.SetColumn("prod_codigo")
END IF
end event

event open;x	= 100
y	= 450

PostEvent("ue_recuperadatos")

istr_mant = Message.PowerObjectParm
istr_mant.argumento[24] = istr_mant.argumento[24]


IF istr_mant.argumento[16]='1' THEN
	dw_1.Enabled	=	False
END IF

IF istr_mant.solo_consulta = True THEN
	dw_1.Enabled	=	False
END IF

dw_1.GetChild("espe_codigo", dw_especies)
dw_1.GetChild("vari_codigo", dw_variedades)
dw_1.GetChild("emba_codigo", dw_embalajes)
dw_1.GetChild("etiq_codigo", dw_etiquetas)
dw_1.GetChild("plde_codigo", dw_plantadesp)
dw_1.GetChild("prod_codigo", dw_productor)
dw_1.GetChild("pafr_calibr", dw_calibres)

dw_especies.SetTransObject(sqlca)
dw_variedades.SetTransObject(sqlca)
dw_embalajes.SetTransObject(sqlca)
dw_etiquetas.SetTransObject(sqlca)
dw_plantadesp.SetTransObject(sqlca)
dw_productor.SetTransObject(sqlca)
dw_calibres.SetTransObject(sqlca)

dw_especies.Retrieve()

dw_variedades.Retrieve(Integer(istr_mant.argumento[3]))
					  							  
dw_embalajes.Retrieve(Integer(istr_mant.Argumento[1]))

dw_etiquetas.Retrieve()

dw_plantadesp.Retrieve(Integer(istr_mant.Argumento[6]))

dw_productor.Retrieve(Integer(istr_mant.Argumento[1]))

dw_calibres.Retrieve(Integer(istr_mant.Argumento[3]), &
							Integer(istr_mant.Argumento[4]))

dw_1.SetTransObject(sqlca)
istr_mant.dw.ShareData(dw_1)

IF istr_mant.Argumento[23] = "1" THEN
	dw_1.Object.vari_codigo.Protect				=	0
	dw_1.Object.emba_codigo.Protect				=	0
	dw_1.Object.vari_codigo.Color					=	0
	dw_1.Object.emba_codigo.Color				=	0
	dw_1.Object.vari_codigo.BackGround.Color		=	RGB(255,255,255)
	dw_1.Object.emba_codigo.BackGround.Color	=	RGB(255,255,255)
ELSE
	dw_1.Object.vari_codigo.Protect				=	1
	dw_1.Object.emba_codigo.Protect				=	1
	dw_1.Object.vari_codigo.Color					=	RGB(255,255,255)
	dw_1.Object.emba_codigo.Color				=	RGB(255,255,255)
	dw_1.Object.vari_codigo.BackGround.Color		=	553648127
	dw_1.Object.emba_codigo.BackGround.Color	=	553648127
END IF
end event

type pb_ultimo from w_mant_detalle_csd`pb_ultimo within w_mant_deta_palletfruta_repa
end type

type pb_siguiente from w_mant_detalle_csd`pb_siguiente within w_mant_deta_palletfruta_repa
end type

type pb_anterior from w_mant_detalle_csd`pb_anterior within w_mant_deta_palletfruta_repa
end type

type pb_primero from w_mant_detalle_csd`pb_primero within w_mant_deta_palletfruta_repa
end type

type pb_cancela from w_mant_detalle_csd`pb_cancela within w_mant_deta_palletfruta_repa
integer x = 2807
integer y = 488
end type

type pb_acepta from w_mant_detalle_csd`pb_acepta within w_mant_deta_palletfruta_repa
integer x = 2807
integer y = 276
end type

event pb_acepta::clicked;istr_mant.respuesta = 1

IF istr_mant.agrega THEN
	Parent.TriggerEvent("ue_nuevo")

//	IF istr_mant.respuesta = 0 THEN
//		pb_salir.TriggerEvent(clicked!)
//	END IF
ELSE
	CloseWithReturn(Parent, istr_mant)
END IF
end event

type pb_salir from w_mant_detalle_csd`pb_salir within w_mant_deta_palletfruta_repa
integer x = 2807
integer y = 708
end type

type dw_1 from w_mant_detalle_csd`dw_1 within w_mant_deta_palletfruta_repa
integer x = 59
integer y = 116
integer width = 2555
integer height = 1276
string dataobject = "dw_mant_palletfruta_repa"
end type

event dw_1::itemchanged;call super::itemchanged;String	ls_columna, ls_nula
Integer  li_lote
Long		ll_Prod
SetNull(ls_Nula)

ls_columna = dwo.name


CHOOSE CASE ls_columna
		
	CASE "emba_codigo"
		istr_mant.Argumento[7]=Data

	CASE "vari_codigo"
		istr_mant.Argumento[4]=Data
			
	CASE "prod_codigo"
		IF  NoExisteProductor(data) THEN
			dw_1.SetItem(il_fila, ls_columna, Long(ls_Nula))
			RETURN 1
			
		ELSEIF istr_mant.Argumento[20]='1' THEN

				dw_1.GetChild("pafr_huert1", idwc_predio)
				idwc_predio.SetTransObject(SQLCA)
				ll_Prod	=	Long(data)
				IF idwc_predio.Retrieve(ll_Prod) = 0 THEN
					idwc_predio.InsertRow(0)
				END IF
				
				IF Varificaproductor(Integer(Data))=FALSE THEN
					RETURN 1
				END IF
						
		END IF
		
	CASE "pafr_huert1" 
		
		IF Not noexistepredio(dw_1.Object.Prod_codigo[row],integer(data)) THEN
			dw_1.SetItem(row, "pafr_huert1", integer(ls_nula))
			Return 1
		ELSE
			dw_1.GetChild("pafr_cuart1", idwc_cuartel)
			idwc_cuartel.SetTransObject(SQLCA)
			idwc_cuartel.Retrieve(dw_1.Object.Prod_codigo[row],integer(data))
			
		END IF
		
	CASE "pafr_calibr"
		IF NoExisteCalibre(data) OR Duplicado(Upper(data)) THEN
			dw_1.SetItem(il_fila, ls_columna, Upper(ias_campo[12]))
			RETURN 1
		END IF

	CASE "pafr_ccajas"
			IF Long(data) < 0 THEN
				dw_1.SetItem(il_fila, ls_columna, Long(istr_mant.Argumento[11]))
				RETURN 1
			END IF
			
			IF Long(data) > Long(istr_mant.Argumento[11]) THEN
				MessageBox("Atención", "Cajas ingresadas sobrepasan las " + istr_mant.Argumento[11] + &
								" cajas del Pallet")
				dw_1.SetItem(il_fila, ls_columna, Long(istr_mant.Argumento[11]))
				RETURN 1
			END IF
		CASE "pafr_fecing"
				IF NoExisteFechaIngreso(data) THEN
					dw_1.SetItem(il_fila, ls_columna,Date( ls_Nula))
					RETURN 1
				END IF
		CASE "pafr_fecemb"
				IF NoExisteFechaEmbalaje(data) THEN
					dw_1.SetItem(il_fila, ls_columna, Date(ls_Nula))
					RETURN 1
				END IF
END CHOOSE
end event

event dw_1::clicked;call super::clicked;String	ls_columna
ls_columna = dwo.name

CHOOSE CASE ls_columna
	CASE "buscaprod"
		buscaprod()
	
END CHOOSE
end event

