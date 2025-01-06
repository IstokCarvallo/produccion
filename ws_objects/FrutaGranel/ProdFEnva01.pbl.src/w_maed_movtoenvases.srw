$PBExportHeader$w_maed_movtoenvases.srw
forward
global type w_maed_movtoenvases from w_mant_encab_deta_csd
end type
type cb_guia from commandbutton within w_maed_movtoenvases
end type
type tab_1 from tab within w_maed_movtoenvases
end type
type tabpage_1 from userobject within tab_1
end type
type dw_encabezado from uo_dw within tabpage_1
end type
type tabpage_1 from userobject within tab_1
dw_encabezado dw_encabezado
end type
type tabpage_2 from userobject within tab_1
end type
type dw_observacion from uo_dw within tabpage_2
end type
type tabpage_2 from userobject within tab_1
dw_observacion dw_observacion
end type
type tab_1 from tab within w_maed_movtoenvases
tabpage_1 tabpage_1
tabpage_2 tabpage_2
end type
end forward

global type w_maed_movtoenvases from w_mant_encab_deta_csd
integer width = 3749
integer height = 2352
string title = "Movimiento de Envases"
string menuname = ""
cb_guia cb_guia
tab_1 tab_1
end type
global w_maed_movtoenvases w_maed_movtoenvases

type variables
w_mant_deta_movtoenvadeta	iw_mantencion_1

Str_mant							istr_mant2

DataWindowChild   			idwc_PltaDest, idwc_Transp, idwc_tipomov, idwc_cama,idwc_planta, idwc_cliente

uo_plantadesp					iuo_PltaDestino
uo_transportista				iuo_Transport
uo_camiones					iuo_Camion
uo_tipomovtofruta				iuo_TipoMovtoEnva
uo_GuiaDespacho				iuo_Guia

Long     	il_NumEnva=0
Integer 	ii_solori, ii_sentido, ii_tipodoc
Boolean	ib_AutoCommit
String		is_rut, is_rutprod, is_nomprod, is_RutProductor, is_NombreProductor, is_chofer
Integer	ii_Cantidad, ii_cliente
long     	il_Productor


end variables

forward prototypes
public function boolean existeprodrut (string as_prodrut)
public subroutine seleccionaproductor ()
public function boolean existemovimiento (integer ai_planta, integer ai_tipomovto, integer ai_numero)
protected function boolean wf_actualiza_db (boolean borrando)
public subroutine buscaproductor ()
public function boolean noexistetipomov (integer ai_tipomov)
public subroutine habilitaingreso (string as_columna)
public subroutine habilitaenca (boolean habilita)
public subroutine buscacamion ()
public function boolean noexistechofer (string rut)
public function boolean noexistecodigoprod (long codigo)
public function boolean noexistecliente (integer cliente)
end prototypes

public function boolean existeprodrut (string as_prodrut);
String	ls_RutProd
Boolean	lb_Retorno	=	True

SELECT	Count(*)
	INTO	:ii_Cantidad
	FROM	dbo.productores
	WHERE	prod_rut	=	:as_ProdRut;

IF SQLCA.SQLCode = -1 THEN
	F_ErrorBaseDatos(SQLCA, "Lectura de Tabla Productores")

	lb_Retorno	=	False
ELSEIF ii_Cantidad = 0 THEN
	MessageBox("Atención", "Productor " + String(as_ProdRut, '@@@.@@@.@@@-@') + &
				", no ha sido registrado.~r~rIngrese o seleccione otro Productor.")

	lb_Retorno	=	False
ELSEIF ii_Cantidad = 1 THEN
	SELECT	prod_codigo, prod_rut, prod_nombre
		INTO	:il_Productor, :is_RutProductor, :is_NombreProductor
		FROM	dbo.productores
		WHERE	prod_rut	=	:as_ProdRut;

	lb_Retorno	=	True
ELSEIF ii_Cantidad > 1 THEN

	lb_Retorno	=	True
END IF

RETURN lb_Retorno
end function

public subroutine seleccionaproductor ();Str_busqueda	lstr_busq

lstr_busq.argum[1] = is_rutprod

OpenWithParm(w_sel_productores, lstr_busq)

lstr_busq	= Message.PowerObjectParm

IF UpperBound(lstr_busq.argum) = 0 THEN
	dw_2.SetColumn("prod_rut")
	dw_2.SetFocus()
ELSE
	dw_2.Object.prod_codigo[il_Fila]	=	Long(lstr_busq.argum[1])
	dw_2.Object.prod_rut[il_Fila]		=	lstr_busq.argum[3]
	dw_2.Object.prod_nombre[il_Fila]	=	lstr_busq.argum[4]
	istr_mant.argumento[6]				=	lstr_busq.argum[1]
	istr_mant.argumento[8]				=	lstr_busq.argum[4]
	dw_2.SetFocus()
END IF

RETURN
end subroutine

public function boolean existemovimiento (integer ai_planta, integer ai_tipomovto, integer ai_numero);Long		ll_Numero, ll_Cliente
Boolean	lb_Retorno = True

 SELECT 	meen_numero, clie_codigo
	INTO	:ll_Numero, :ll_Cliente
	FROM	dbo.spro_movtoenvaenca
	WHERE	plde_codigo	=	:ai_Planta
	AND	tpmv_codigo	=	:ai_TipoMovto
	AND	meen_numero	=	:ai_Numero ;
	
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de Tabla de Movimiento de Envases")
	
	lb_Retorno	=	False
ELSEIF sqlca.SQLCode = 0 THEN
	istr_mant.Argumento[3]		=	String(ai_Numero)
	istr_mant.Argumento[16]	=	String(ll_Cliente)
	ii_cliente 						= 	ll_Cliente
	This.TriggerEvent("ue_recuperadatos")
ELSE
	MessageBox("Atención","Número de Correlativo No ha sido generado. Ingrese Otro.")
	lb_Retorno	=	False
END IF

RETURN lb_Retorno
end function

protected function boolean wf_actualiza_db (boolean borrando);Boolean	lb_AutoCommit, lb_Retorno

IF Not dw_2.uf_check_required(0) THEN RETURN False
IF Not dw_1.uf_validate(0) THEN RETURN False

lb_AutoCommit		=	sqlca.AutoCommit
sqlca.AutoCommit	=	False

IF NOT Borrando THEN
	IF dw_2.GetItemStatus(1, 0, Primary!) = NewModified! THEN
		dw_2.SetItem(1,"meen_usuari",gstr_us.nombre)
		dw_2.SetItem(1,"meen_comput",gstr_us.computador)
		dw_2.SetItem(1,"meen_horacr",F_Fechahora())
	ELSEIF dw_2.GetItemStatus(1, 0, Primary!) = DataModified! THEN
		dw_2.SetItem(1,"meen_usumod",gstr_us.nombre)
		dw_2.SetItem(1,"meen_commod",gstr_us.computador)
		dw_2.SetItem(1,"meen_horact",F_Fechahora())
	END IF
END IF

IF Borrando THEN
	IF dw_1.Update(True, False) = -1 THEN
		F_ErrorBaseDatos(sqlca, This.Title)
	ELSEIF dw_2.Update(True, False) = -1 THEN
		F_ErrorBaseDatos(sqlca, This.Title)
		
		RollBack;
	ELSE
		Commit;
		
		IF sqlca.SQLCode <> 0 THEN
			F_ErrorBaseDatos(sqlca, This.Title)
		ELSE
			lb_Retorno	=	True
			
			dw_2.ResetUpdate()
			dw_1.ResetUpdate()
		END IF
	END IF
ELSE
	IF dw_2.Update(True, False) = -1 THEN
		F_ErrorBaseDatos(sqlca, This.Title)
	ELSEIF dw_1.Update(True, False) = -1 THEN
		F_ErrorBaseDatos(sqlca, This.Title)
		
		RollBack;
	ELSE
		Commit;
		
		IF sqlca.SQLCode <> 0 THEN
			F_ErrorBaseDatos(sqlca, This.Title)
		ELSE
			lb_Retorno	=	True
			
			dw_2.ResetUpdate()
			dw_1.ResetUpdate()
		END IF
	END IF
END IF

sqlca.AutoCommit	=	lb_AutoCommit

RETURN lb_Retorno
end function

public subroutine buscaproductor ();Str_busqueda	lstr_busq

OpenWithParm(w_busc_productores, lstr_busq)

lstr_busq	= Message.PowerObjectParm

IF lstr_busq.argum[1] = "" THEN
	dw_2.SetColumn("prod_rut")
	dw_2.SetFocus()
ELSE
	dw_2.Object.prod_codigo[il_fila]	=	Long(lstr_busq.argum[1])
	dw_2.Object.prod_nombre[il_fila]	=	lstr_busq.argum[2]
	
	dw_2.Object.prod_rut[il_fila]		=	lstr_busq.argum[8]
	istr_Mant.Argumento[6]				=	lstr_busq.argum[1]
	istr_mant.argumento[8]				=	lstr_busq.argum[2]
	
	HabilitaIngreso("prod_codigo")
	dw_2.SetFocus()
END IF

RETURN
end subroutine

public function boolean noexistetipomov (integer ai_tipomov);Integer	li_Contador
String	ls_nombre

SELECT	tpmv_nombre, tpmv_solori, tpmv_sentid, tdop_codigo
	INTO	:ls_nombre, : ii_solori, :ii_sentido, :ii_tipodoc
	FROM	dbo.spro_tipomovtofruta
	WHERE	tpmv_codigo	=	:ai_tipomov
	AND	tpmv_frugra	=	0
	AND	tpmv_fruemb	=	0
	AND	tpmv_frucom	=	0
	AND	tpmv_envase	=	1;
	
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura tabla Tipo de Movimiento de Fruta")
	
	RETURN True
ELSEIF sqlca.SQLCode = 100 THEN
		MessageBox("Atención", "Código de Tipo de Movimiento de Fruta (" + String(ai_tipomov, '000') + "), no ha sido~r" + &
		"ingresado o no pertenece a Tipo de Movimiento de Envases.~r~rIngrese o seleccione otro Código.")
		
	RETURN True
END IF

RETURN False
end function

public subroutine habilitaingreso (string as_columna);Boolean	lb_Estado = True
Date		ld_Fecha

If as_Columna <> "meen_fecmov" AND &
	(dw_2.Object.meen_fecmov[1] = ld_Fecha OR IsNull(dw_2.Object.meen_fecmov[1])) Then
	lb_Estado = False
End If
	
If (istr_Mant.Argumento[2] = "42") OR (istr_Mant.Argumento[2] = "62") Then
	If as_Columna <> "plde_coorde" AND &
		(dw_2.Object.plde_coorde[1] = 0 OR IsNull(dw_2.Object.plde_coorde[1])) Then
		lb_Estado = False
	End If
End If
	
If as_Columna <> "tran_codigo" AND &
	(dw_2.Object.tran_codigo[1] = 0 OR IsNull(dw_2.Object.tran_codigo[1])) Then
	lb_Estado = False
End If
	
If as_Columna <> "meen_rutcho" AND &
	(dw_2.Object.meen_rutcho[1] = "" OR IsNull(dw_2.Object.meen_rutcho[1])) Then
	lb_Estado = False
End If

If as_Columna <> "meen_chofer" AND &
	(dw_2.Object.meen_chofer[1] = "" OR IsNull(dw_2.Object.meen_chofer[1])) Then
	lb_Estado = False
End If

dw_1.Enabled	=	lb_Estado
pb_ins_det.Enabled	=	lb_Estado
end subroutine

public subroutine habilitaenca (boolean habilita);IF Habilita THEN
	Tab_1.Tabpage_1.dw_Encabezado.Object.plde_codigo.Protect				=	0
	Tab_1.Tabpage_1.dw_Encabezado.Object.clie_codigo.Protect				=	0
	Tab_1.Tabpage_1.dw_Encabezado.Object.tpmv_codigo.Protect			=	0
	Tab_1.Tabpage_1.dw_Encabezado.Object.meen_numero.Protect			=	0
	Tab_1.Tabpage_1.dw_Encabezado.Object.prod_codigo.Protect           	=  0
	Tab_1.Tabpage_1.dw_Encabezado.Object.prod_rut.Protect          		=  0
	Tab_1.Tabpage_1.dw_Encabezado.Object.meen_guisii.Protect				=	0
	Tab_1.Tabpage_1.dw_Encabezado.Object.meen_fecmov.Protect			=	0
	Tab_1.Tabpage_1.dw_Encabezado.Object.cami_clasifi.Protect				=	0
	Tab_1.Tabpage_1.dw_Encabezado.Object.tran_codigo.Protect				=	0
	Tab_1.Tabpage_1.dw_Encabezado.Object.cami_patent.Protect				=	0
	Tab_1.Tabpage_1.dw_Encabezado.Object.cami_patcar.Protect				=	0
	Tab_1.Tabpage_1.dw_Encabezado.Object.meen_rutcho.Protect			=	0
	Tab_1.Tabpage_1.dw_Encabezado.Object.meen_chofer.Protect			=	0

	Tab_1.Tabpage_1.dw_Encabezado.Object.plde_codigo.Color		=	0
	Tab_1.Tabpage_1.dw_Encabezado.Object.clie_codigo.Color		=	0
	Tab_1.Tabpage_1.dw_Encabezado.Object.tpmv_codigo.Color		=	0
	Tab_1.Tabpage_1.dw_Encabezado.Object.meen_numero.Color	=	0
	Tab_1.Tabpage_1.dw_Encabezado.Object.prod_codigo.Color  		=	0  
   	Tab_1.Tabpage_1.dw_Encabezado.Object.prod_rut.Color 			=  0
	Tab_1.Tabpage_1.dw_Encabezado.Object.prod_nombre.Color  	=  0
	Tab_1.Tabpage_1.dw_Encabezado.Object.meen_guisii.Color		=	0
	Tab_1.Tabpage_1.dw_Encabezado.Object.meen_fecmov.Color		=	0
	Tab_1.Tabpage_1.dw_Encabezado.Object.tran_codigo.Color		=	0
	Tab_1.Tabpage_1.dw_Encabezado.Object.cami_patent.Color		=	0
	Tab_1.Tabpage_1.dw_Encabezado.Object.cami_patcar.Color		=	0
	Tab_1.Tabpage_1.dw_Encabezado.Object.meen_rutcho.Color		=	0
	Tab_1.Tabpage_1.dw_Encabezado.Object.meen_chofer.Color		=	0

	Tab_1.Tabpage_1.dw_Encabezado.Object.plde_codigo.BackGround.Color		=	RGB(255,255,255)
	Tab_1.Tabpage_1.dw_Encabezado.Object.clie_codigo.BackGround.Color			=	RGB(255,255,255)	
	Tab_1.Tabpage_1.dw_Encabezado.Object.tpmv_codigo.BackGround.Color		=	RGB(255,255,255)
	Tab_1.Tabpage_1.dw_Encabezado.Object.meen_numero.BackGround.Color	=	RGB(255,255,255)
	Tab_1.Tabpage_1.dw_Encabezado.Object.prod_codigo.BackGround.Color  		=  RGB(255,255,255)
   	Tab_1.Tabpage_1.dw_Encabezado.Object.prod_rut.BackGround.Color 			=  RGB(255,255,255)
	Tab_1.Tabpage_1.dw_Encabezado.Object.prod_nombre.BackGround.Color  	=  RGB(255,255,255)
	Tab_1.Tabpage_1.dw_Encabezado.Object.meen_guisii.BackGround.Color		=	RGB(255,255,255)
	Tab_1.Tabpage_1.dw_Encabezado.Object.meen_fecmov.BackGround.Color		=	RGB(255,255,255)
	Tab_1.Tabpage_1.dw_Encabezado.Object.tran_codigo.BackGround.Color		=	RGB(255,255,255)
	Tab_1.Tabpage_1.dw_Encabezado.Object.cami_patent.BackGround.Color		=	RGB(255,255,255)
	Tab_1.Tabpage_1.dw_Encabezado.Object.cami_patcar.BackGround.Color		=	RGB(255,255,255)
	Tab_1.Tabpage_1.dw_Encabezado.Object.meen_rutcho.BackGround.Color		=	RGB(255,255,255)
	Tab_1.Tabpage_1.dw_Encabezado.Object.meen_chofer.BackGround.Color		=	RGB(255,255,255)
ELSE
	Tab_1.Tabpage_1.dw_Encabezado.Object.plde_codigo.Protect				=	1
	Tab_1.Tabpage_1.dw_Encabezado.Object.clie_codigo.Protect				=	1
	Tab_1.Tabpage_1.dw_Encabezado.Object.tpmv_codigo.Protect			=	1
	Tab_1.Tabpage_1.dw_Encabezado.Object.meen_numero.Protect			=	1
	Tab_1.Tabpage_1.dw_Encabezado.Object.plde_coorde.Protect				=	1
	Tab_1.Tabpage_1.dw_Encabezado.Object.prod_codigo.Protect           	=  1
	Tab_1.Tabpage_1.dw_Encabezado.Object.prod_rut.Protect           		=  1
	Tab_1.Tabpage_1.dw_Encabezado.Object.meen_guisii.Protect				=	1
	Tab_1.Tabpage_1.dw_Encabezado.Object.meen_fecmov.Protect			=	1
	Tab_1.Tabpage_1.dw_Encabezado.Object.cami_clasifi.Protect				=	1
	Tab_1.Tabpage_1.dw_Encabezado.Object.tran_codigo.Protect				=	1
	Tab_1.Tabpage_1.dw_Encabezado.Object.cami_patent.Protect				=	1
	Tab_1.Tabpage_1.dw_Encabezado.Object.cami_patcar.Protect				=	1
	Tab_1.Tabpage_1.dw_Encabezado.Object.meen_rutcho.Protect			=	1
	Tab_1.Tabpage_1.dw_Encabezado.Object.meen_chofer.Protect			=	1

	Tab_1.Tabpage_1.dw_Encabezado.Object.plde_codigo.Color		=	RGB(255,255,255)
	Tab_1.Tabpage_1.dw_Encabezado.Object.clie_codigo.Color		=	RGB(255,255,255)
	Tab_1.Tabpage_1.dw_Encabezado.Object.tpmv_codigo.Color		=	RGB(255,255,255)
	Tab_1.Tabpage_1.dw_Encabezado.Object.meen_numero.Color	=	RGB(255,255,255)
	Tab_1.Tabpage_1.dw_Encabezado.Object.prod_codigo.Color 		=  RGB(255,255,255)
   	Tab_1.Tabpage_1.dw_Encabezado.Object.prod_rut.Color 			=  RGB(255,255,255)
	Tab_1.Tabpage_1.dw_Encabezado.Object.prod_nombre.Color  	=  RGB(255,255,255)
	Tab_1.Tabpage_1.dw_Encabezado.Object.meen_guisii.Color		=	RGB(255,255,255)
	Tab_1.Tabpage_1.dw_Encabezado.Object.meen_fecmov.Color		=	RGB(255,255,255)
	Tab_1.Tabpage_1.dw_Encabezado.Object.tran_codigo.Color		=	RGB(255,255,255)
	Tab_1.Tabpage_1.dw_Encabezado.Object.cami_patent.Color		=	RGB(255,255,255)
	Tab_1.Tabpage_1.dw_Encabezado.Object.cami_patcar.Color		=	RGB(255,255,255)
	Tab_1.Tabpage_1.dw_Encabezado.Object.meen_rutcho.Color		=	RGB(255,255,255)
	Tab_1.Tabpage_1.dw_Encabezado.Object.meen_chofer.Color		=	RGB(255,255,255)
	
	Tab_1.Tabpage_1.dw_Encabezado.Object.plde_codigo.BackGround.Color		=	553648127
	Tab_1.Tabpage_1.dw_Encabezado.Object.clie_codigo.BackGround.Color			=	553648127	
	Tab_1.Tabpage_1.dw_Encabezado.Object.tpmv_codigo.BackGround.Color		=	553648127
	Tab_1.Tabpage_1.dw_Encabezado.Object.meen_numero.BackGround.Color	=	553648127
	Tab_1.Tabpage_1.dw_Encabezado.Object.plde_coorde.BackGround.Color		=	553648127
	Tab_1.Tabpage_1.dw_Encabezado.Object.prod_codigo.BackGround.Color 		=  553648127
	Tab_1.Tabpage_1.dw_Encabezado.Object.prod_rut.BackGround.Color 			=  553648127
	Tab_1.Tabpage_1.dw_Encabezado.Object.prod_nombre.BackGround.Color  	=  553648127
	Tab_1.Tabpage_1.dw_Encabezado.Object.meen_guisii.BackGround.Color		=	553648127
	Tab_1.Tabpage_1.dw_Encabezado.Object.meen_fecmov.BackGround.Color		=	553648127
	Tab_1.Tabpage_1.dw_Encabezado.Object.tran_codigo.BackGround.Color		=	553648127
	Tab_1.Tabpage_1.dw_Encabezado.Object.cami_patent.BackGround.Color		=	553648127
	Tab_1.Tabpage_1.dw_Encabezado.Object.cami_patcar.BackGround.Color		=	553648127
	Tab_1.Tabpage_1.dw_Encabezado.Object.meen_rutcho.BackGround.Color		=	553648127
	Tab_1.Tabpage_1.dw_Encabezado.Object.meen_chofer.BackGround.Color		=	553648127
END IF
end subroutine

public subroutine buscacamion ();Str_busqueda	lstr_busq

lstr_busq.argum[1] = istr_Mant.Argumento[6]

OpenWithParm(w_busc_camiones, lstr_busq)

lstr_busq	= Message.PowerObjectParm

IF lstr_busq.argum[1] = "" THEN
	dw_2.SetColumn("cami_patent")
	dw_2.SetFocus()
ELSE
	dw_2.Object.cami_patent[il_fila]		=	lstr_busq.argum[2]
	dw_2.Object.cami_patcar[il_fila]		=	lstr_busq.argum[6]
	dw_2.Object.meen_rutcho[il_fila]		=	lstr_busq.argum[5]
	dw_2.Object.meen_chofer[il_fila]		=	lstr_busq.argum[4]

	HabilitaIngreso("cami_patent")
	
	dw_2.SetFocus()
END IF

RETURN
end subroutine

public function boolean noexistechofer (string rut);Integer	li_Choferes

Select	Count(cami_rutcho)
into		:li_Choferes
From		dbo.camiones
Where		cami_rutcho = :rut;

IF li_Choferes = 1 THEN
	SELECT 	cami_chofer
	INTO		:is_chofer
	FROM 		dbo.camiones
	WHERE		cami_rutcho =:rut;

	IF sqlca.SQLCode = -1 THEN
		F_ErrorBaseDatos(sqlca, "Lectura de Tabla ClienProve")
		Return True
	END IF
END IF

Return False
end function

public function boolean noexistecodigoprod (long codigo);	SELECT prod_rut,prod_nombre
	INTO	:is_rutprod,:is_nomprod
	FROM	dbo.productores
	WHERE	prod_codigo	=	:codigo;
	
	
	IF sqlca.SQLCode = -1 THEN
			F_ErrorBaseDatos(sqlca,"Lectura de la Tabla Productores" )
			Return True			
	ELSEIF sqlca.SQLCode = 100 THEN
			messagebox("Atención","No Existe Código Seleccionado en Tabla ")			
			Return True						
	END IF

	Return False

end function

public function boolean noexistecliente (integer cliente);String ls_nombre

SELECT 	clie_nombre
INTO		:ls_nombre
FROM 		dbo.clientesprod
WHERE		clie_codigo =:cliente;

IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de Tabla ClientesProd")
	Return True
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención","Código No ha sido Generado. Ingrese Otro.")
	Return True
END IF

Return False
end function

event open;x				= 0   
y				= 0
This.Height	= 2520
im_menu		= m_principal

This.ParentWindow().ToolBarVisible	= True
im_menu.Item[1].Item[6].Enabled		= True
im_menu.Item[7].Visible					= True

istr_Mant.Argumento[1]	=	String(gstr_ParamPlanta.CodigoPlanta)
istr_Mant.Argumento[2]	=	"41" 
istr_Mant.Argumento[3]	=	""
istr_Mant.Argumento[4]	=	"1"
istr_Mant.Argumento[5]  =  "4"  // Productor
istr_Mant.Argumento[6]	=  ""
istr_Mant.Argumento[7]	=	"1"
istr_Mant.Argumento[8]	=	""
istr_Mant.Argumento[9]	=	""
istr_Mant.Argumento[10]	=	""

dw_2.Object.plde_coorde.Protect				=	1
dw_2.Object.plde_coorde.Color				=	Rgb(255,255,255)
dw_2.Object.plde_coorde.BackGround.Color	=	553648127

dw_2.GetChild("plde_codigo", idwc_planta)
idwc_planta.SetTransObject(sqlca)
idwc_planta.Retrieve(gi_codexport)

dw_2.GetChild("plde_coorde", idwc_PltaDest)
idwc_PltaDest.SetTransObject(sqlca)

dw_2.GetChild("clie_codigo", idwc_cliente)
idwc_cliente.SetTransObject(SQLCA)
idwc_cliente.Retrieve()

If idwc_PltaDest.Retrieve(gi_codexport) = 0 Then
	MessageBox("Atención", "Falta Registrar Plantas")
	idwc_PltaDest.InsertRow(0)
Else
//	idwc_PltaDest.SetFilter("plde_codigo <> " + String(gstr_ParamPlanta.CodigoPlanta))
//	idwc_PltaDest.Filter()
	idwc_PltaDest.SetSort("plde_nombre A")
	idwc_PltaDest.Sort()
End If

dw_2.GetChild("tpmv_codigo", idwc_tipomov)
idwc_tipomov.SetTransObject(sqlca)

If idwc_tipomov.Retrieve() = 0 Then
	MessageBox("Atención", "Falta Registrar Tipo de Movimiento")
	idwc_tipomov.InsertRow(0)
Else
	idwc_tipomov.SetFilter("tpmv_frugra=0 and tpmv_fruemb=0 and tpmv_frucom=0 and tpmv_envase=1 ")
	idwc_tipomov.Filter()
	idwc_tipomov.SetSort("tpmv_nombre A")
	idwc_tipomov.Sort()
End If

dw_2.GetChild("tran_codigo", idwc_Transp)
idwc_Transp.SetTransObject(sqlca)

If idwc_Transp.Retrieve() = 0 Then
	MessageBox("Atención", "Falta Registrar Transportístas")
	idwc_Transp.InsertRow(0)
Else
	idwc_Transp.SetSort("tran_nombre A")
	idwc_Transp.Sort()
End If

dw_1.SetTransObject(sqlca)
dw_2.SetTransObject(sqlca)

istr_Mant.dw				=	dw_1
istr_Mant.solo_consulta 	=	False

pb_nuevo.PostEvent(Clicked!)

iuo_PltaDestino			=	Create uo_plantadesp
iuo_Transport			=	Create uo_transportista
iuo_Camion				=	Create uo_camiones
iuo_TipoMovtoEnva	=	Create uo_tipomovtofruta
iuo_Guia					=	Create uo_GuiaDespacho

buscar	= "Planta:Nplde_codigo,Tipo Movto:Ntpmv_codigo, Numero:Nmeen_numero"
ordenar	= "Planta:plde_codigo,Tipo Movto:tpmv_codigo, Numero:meen_numero"

end event

on w_maed_movtoenvases.create
int iCurrent
call super::create
this.cb_guia=create cb_guia
this.tab_1=create tab_1
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.cb_guia
this.Control[iCurrent+2]=this.tab_1
end on

on w_maed_movtoenvases.destroy
call super::destroy
destroy(this.cb_guia)
destroy(this.tab_1)
end on

event ue_nuevo;is_rut 		=	""
is_rutprod	=	""

Call Super::ue_nuevo 

dw_2.Object.plde_codigo[1]		=	gstr_ParamPlanta.CodigoPlanta
dw_2.Object.tpmv_codigo[1]	=	Integer(istr_Mant.Argumento[2])
dw_2.Object.meen_fecmov[1]	=	Today()
dw_2.Object.clie_codigo[1] 		=  gi_CodExport
istr_Mant.Argumento[16]		=  String(gi_CodExport)

istr_Mant.Argumento[3]	=	''
Habilitaenca(True)

cb_guia.Enabled = False

dw_2.ShareData(Tab_1.TabPage_1.dw_Encabezado)
dw_2.ShareData(Tab_1.TabPage_2.dw_observacion)

If Not NoExisteTipoMov(dw_2.Object.tpmv_codigo[1]) Then
	If ii_tipodoc=2 Then 
		dw_2.Object.meen_guisii.Protect				=	1
		dw_2.Object.meen_guisii.Color					=	RGB(255,255,255)
		dw_2.Object.meen_guisii.BackGround.Color	=	553648127
	Else
		dw_2.Object.meen_guisii.Protect				=	0
		dw_2.Object.meen_guisii.Color					=	0
		dw_2.Object.meen_guisii.BackGround.Color	=	RGB(255,255,255)
	End If
End If

end event

event ue_recuperadatos;Long		ll_fila_e, respuesta, ll_fila_env
Integer	li_Sentido

DO
	dw_2.SetRedraw(False)
	dw_2.Reset()
	ll_fila_e	= dw_2.Retrieve(Integer(istr_mant.argumento[1]),Integer(istr_mant.argumento[2]),&
										 Long(istr_mant.argumento[3]), Integer(istr_Mant.Argumento[16]))
	If ll_fila_e = -1 Then
		respuesta = MessageBox("Error en Base de Datos", "No es posible conectar la Base de Datos.", &
										Information!, RetryCancel!)
	ElseIf ll_fila_e > 0 Then
		HabilitaEnca(False)
		DO	
			If dw_2.Object.tpmv_codrec[1]	= 1 Then
				li_Sentido	=	0
				istr_mant.Solo_Consulta	=	True
			Else
				li_Sentido	=	Integer(istr_mant.argumento[4])
				istr_mant.Solo_Consulta	=	False
			End If
			
			If dw_1.Retrieve(Integer(istr_mant.argumento[1]),Integer(istr_mant.argumento[2]),&
							     	  Long(istr_mant.argumento[3]),li_Sentido, Integer(istr_Mant.Argumento[16])) = -1 Then
				respuesta = MessageBox("Error en Base de Datos", "No es posible conectar la Base de Datos.", Information!, RetryCancel!)
			Else
				If dw_2.Object.meen_guiemi[1] = 1 Then
					pb_eliminar.PictureName 	= "\Desarrollo 17\Imagenes\Botones\Nulo.png"
					pb_eliminar.DisabledName 	= "\Desarrollo 17\Imagenes\Botones\Nulo-BN.png"

					cb_guia.Enabled			= False
					pb_eliminar.Enabled		= True
					pb_grabar.Enabled		= True
				ElseIf dw_2.Object.meen_guiemi[1] = 3 Then
					pb_eliminar.PictureName 	= "\Desarrollo 17\Imagenes\Botones\Nulo.png"
					pb_eliminar.DisabledName 	= "\Desarrollo 17\Imagenes\Botones\Nulo-BN.png"

					cb_guia.Enabled			= True
					pb_eliminar.Enabled		= True
					pb_grabar.Enabled		= True
				ElseIf dw_2.Object.meen_guiemi[1] = 2 Then
					cb_guia.Enabled			= False
					pb_eliminar.Enabled		= False
					pb_grabar.Enabled		= False
				Else
					pb_eliminar.PictureName 	= "\Desarrollo 17\Imagenes\Botones\Basurero.png"
					pb_eliminar.DisabledName 	= "\Desarrollo 17\Imagenes\Botones\Basurero-BN.png"
				
					pb_ins_det.Enabled	= True
					pb_eli_det.Enabled	= True
					pb_eliminar.Enabled	= True
					pb_grabar.Enabled	= True
					
					dw_1.SetRow(1)
					dw_1.SelectRow(1,True)
					dw_1.SetFocus()
				End If
				
				If Not NoExisteTipoMov(dw_2.Object.tpmv_codigo[1]) Then
					istr_mant.argumento[2] = String(dw_2.Object.tpmv_codigo[1])
					istr_mant.argumento[4] = string(ii_sentido)
					istr_mant.argumento[5] = string(ii_solori)
					
					If ii_solori <> 2 Then
						dw_2.Object.plde_coorde.Protect					=	0
						dw_2.Object.plde_coorde.Color					=	0
						dw_2.Object.plde_coorde.BackGround.Color	=	RGB(255,255,255)
						
						dw_2.Object.prod_rut.Protect           				=  1
						
						dw_2.Object.prod_rut.Color  						=  RGB(255,255,255)
						dw_2.Object.prod_nombre.Color  					=  RGB(255,255,255)
						dw_2.Object.prod_rut.BackGround.Color  		=  553648127
						dw_2.Object.prod_nombre.BackGround.Color  	=  553648127
					Else
						dw_2.Object.plde_coorde.Protect					=	1
						dw_2.Object.plde_coorde.Color					=	RGB(255,255,255)
						dw_2.Object.plde_coorde.BackGround.Color	=	553648127
						
						dw_2.Object.prod_rut.Protect           				=  0
						dw_2.Object.prod_rut.Color  						=  0
						dw_2.Object.prod_nombre.Color  					=  0
						dw_2.Object.prod_rut.BackGround.Color  		=  RGB(255,255,255)
						dw_2.Object.prod_nombre.BackGround.Color  	=  RGB(255,255,255)
					End If
		
					If ii_tipodoc=2 Then
						dw_2.Object.meen_guisii.Protect				=	1
						dw_2.Object.meen_guisii.Color					=	RGB(255,255,255)
						dw_2.Object.meen_guisii.BackGround.Color	=	553648127
					Else
						dw_2.Object.meen_guisii.Protect				=	0
						dw_2.Object.meen_guisii.Color					=	0
						dw_2.Object.meen_guisii.BackGround.Color	=	RGB(255,255,255)
					End If
				End If
				
				pb_imprimir.Enabled		= True
				
			End If
		LOOP WHILE respuesta = 1

		If respuesta = 2 Then Close(This)
	End If
	dw_2.SetRedraw(True)
LOOP WHILE respuesta = 1

If respuesta = 2 Then Close(This)

end event

event ue_antesguardar;Long		ll_fila, ll_guia
Boolean  lb_Actualiza_Envase = FALSE			
Integer	li_Planta, li_TipoMovto

Message.DoubleParm = 0

li_Planta			=	Integer(istr_mant.Argumento[1])
li_TipoMovto	=	Integer(istr_mant.Argumento[2])
ll_guia           	=  dw_2.object.meen_guisii[1]

If Integer(istr_mant.Argumento[4])=2 And (Isnull(ll_guia) or ll_guia=0) Then
	If ii_tipodoc<>2 Then
   		MessageBox("Guía de Despacho", "Debe Ingresar el Número de la Guía de Despacho.", Information!, OK!)
		Message.DoubleParm = -1
		Return
	End If	
End If	

ib_AutoCommit			=	SQLCA.AutoCommit
SQLCA.AutoCommit	=	False

If dw_2.GetItemStatus(1, 0, Primary!) = NewModified! Then
	If il_NumEnva=0 Then
		iuo_TipoMovtoEnva.bloqueacorrel()	
		il_NumEnva = iuo_TipoMovtoEnva.UltimoCorrelativo(4,li_TipoMovto,li_Planta) 
	
		If il_NumEnva = 0 OR IsNull(il_NumEnva) Then
			Messagebox("Error de Conección","Vuelva a Intentar Grabar.")
			Message.DoubleParm = -1
			RETURN
		Else
			lb_Actualiza_Envase = TRUE
		End If
	End If
	
	dw_2.Object.meen_numero[1]	=	il_NumEnva
	istr_mant.Argumento[3]		=	String(il_NumEnva)
	
	  	//Preguntar el Momento de Actualización
   If lb_Actualiza_Envase Then iuo_TipoMovtoEnva.Actualiza_Correlativo(4,li_Planta,li_TipoMovto,il_NumEnva) 
      ///////////////////////////////////////
Else
	istr_mant.Argumento[3]	=	String(dw_2.Object.meen_numero[1])
End If

For ll_fila = 1 TO dw_1.RowCount()
	dw_1.SetItem(ll_fila, "plde_codigo", dw_2.Object.plde_codigo[1])
	dw_1.SetItem(ll_fila, "tpmv_codigo", dw_2.Object.tpmv_codigo[1])
	dw_1.SetItem(ll_fila, "meen_numero", Long(istr_mant.Argumento[3]))
	dw_1.SetItem(ll_fila, "fgme_sentid", istr_mant.Argumento[4])
Next

If ii_tipodoc=2 AND (isnull(ll_guia) OR ll_guia=0) Then cb_guia.Enabled	=	True
end event

event ue_modifica_detalle();
istr_mant.agrega	= False
istr_mant.borra	= False


IF dw_1.RowCount() > 0 THEN
	istr_mant.dw	=	dw_1
	OpenWithParm(iw_mantencion_1, istr_mant)
END IF


end event

event ue_nuevo_detalle;integer li_i
istr_mant.Borra	=	False
istr_mant.Agrega	=	True
	

istr_mant.dw	=	dw_1
istr_mant.Argumento[17] = String(dw_2.Object.prod_codigo[il_fila])
istr_mant.Argumento[18] = dw_2.Object.prod_nombre[il_fila]
OpenWithParm(iw_mantencion_1, istr_mant)
	
IF dw_1.RowCount() > 0  THEN
	FOR li_i = 1 to dw_1.RowCount()
		dw_1.Object.Clie_codigo[li_i] = Integer(istr_mant.Argumento[16])
	NEXT
	pb_eliminar.Enabled	=	True
	pb_grabar.Enabled		=	True
	pb_eli_det.Enabled	=	True
ELSE
	pb_eliminar.Enabled	=	False
	pb_grabar.Enabled		=	False
	pb_eli_det.Enabled	=	False
END IF

end event

event ue_borra_detalle;call super::ue_borra_detalle;Integer	li_borra

SetPointer(HourGlass!)

ib_borrar = True
w_main.SetMicroHelp("Validando la eliminación de detalle...")

Message.DoubleParm = 0

This.TriggerEvent ("ue_validaborrar_detalle")

If Message.DoubleParm = -1 Then RETURN

istr_mant.borra	= True
istr_mant.agrega	= False

istr_mant.dw	=	dw_1
OpenWithParm(iw_mantencion_1, istr_mant)

istr_mant = Message.PowerObjectParm

If istr_mant.respuesta = 1 Then
	li_borra	=	dw_1.DeleteRow(0)
	
	If li_borra = 1 Then
		ib_borrar = False
		w_main.SetMicroHelp("Borrando Registro...")
		SetPointer(Arrow!)
	Else
		ib_borrar = False
		MessageBox(This.Title,"No se puede borrar actual registro.")
	End If
End If

istr_mant.borra	 = False

If dw_1.RowCount()<=0 Then
	pb_eliminar.Enabled	=	False
	pb_grabar.Enabled	=	False
	pb_eli_det.Enabled	=	False
End If	
end event

event ue_imprimir;SetPointer(HourGlass!)

Long		fila
Integer  li_Sentido

istr_info.titulo	= "MOVIMIENTO DE ENVASES"
istr_info.copias	= 1

OpenWithParm(vinf,istr_info)
vinf.dw_1.DataObject = "dw_info_movtoenvases"
vinf.dw_1.SetTransObject(sqlca)

If dw_2.Object.tpmv_codrec[1]	= 1 Then
	li_Sentido	=	0
Else
	li_Sentido	=	Integer(istr_mant.argumento[4])
End If

fila = vinf.dw_1.Retrieve(Integer(istr_mant.Argumento[1]),Integer(istr_mant.Argumento[2]),&
								  Integer(istr_mant.Argumento[3]),li_Sentido)

If fila = -1 Then
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ElseIf fila = 0 Then
	MessageBox( "No Existe información", "No existe información para este informe.", StopSign!, Ok!)
Else
	F_Membrete(vinf.dw_1)
	If gs_Ambiente <> 'Windows' Then F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
End If

SetPointer(Arrow!)
end event

event ue_seleccion;call super::ue_seleccion;istr_busq.argum[1] = ""
istr_busq.argum[2] = ""
istr_busq.argum[3] = ""
istr_busq.argum[23] = ""

istr_Busq.Argum[1]	=	String(gstr_ParamPlanta.CodigoPlanta)
istr_Busq.Argum[2]	=	istr_Mant.Argumento[2]
istr_Busq.Argum[33]	=  String(dw_2.Object.clie_codigo[1])

OpenWithParm(w_busc_spro_movtoenvase, istr_busq)
istr_Busq	= Message.PowerObjectParm

IF istr_Busq.Argum[3] <> "" THEN
	istr_Mant.Argumento[1] 	= istr_Busq.Argum[1]
	istr_Mant.Argumento[2] 	= istr_Busq.Argum[2]
	istr_Mant.Argumento[3] 	= istr_Busq.Argum[3]
	ii_cliente 					= Integer(istr_Busq.Argum[33])
	IF NoExisteTipoMov(Integer(istr_Mant.Argumento[2])) THEN
		RETURN 
	ElSE
		istr_mant.argumento[4] = string(ii_sentido)
		istr_mant.argumento[5] = string(ii_solori)
	END IF
	
	TriggerEvent("ue_recuperadatos")
ELSE
	pb_buscar.SetFocus()
END IF
end event

event ue_guardar();IF dw_1.AcceptText() = -1 THEN RETURN

SetPointer(HourGlass!)

w_main.SetMicroHelp("Grabando información...")

Message.DoubleParm = 0

TriggerEvent("ue_antesguardar")

IF Message.DoubleParm = -1 THEN RETURN

IF wf_actualiza_db(False) THEN
	w_main.SetMicroHelp("Información Grabada.")
	pb_eliminar.Enabled	= True
	pb_imprimir.Enabled	= True
ELSE
	w_main.SetMicroHelp("No se puede Grabar información.")
	Message.DoubleParm = -1
	RETURN
END IF

IF il_NumEnva>0 AND dw_2.GetItemStatus(1, 0, Primary!) = NotModified! THEN
	il_NumEnva	= 0
END IF
end event

event resize;call super::resize;Tab_1.x = dw_2.x
Tab_1.y = dw_2.y

dw_1.y					=	64 + Tab_1.Height
dw_1.Height				=	This.WorkSpaceHeight() - dw_1.y - 41

cb_guia.y				=	pb_Salir.y + 255
cb_guia.x				=	pb_Salir.x
end event

event ue_borrar;If dw_2.RowCount() < 1 Then Return

SetPointer(HourGlass!)

ib_borrar = True
w_main.SetMicroHelp("Validando la eliminación...")

Message.DoubleParm = 0

This.TriggerEvent ("ue_validaborrar")

If Message.DoubleParm = -1 Then Return

If dw_1.RowCount() > 0 Then dw_1.RowsMove(1,dw_1.RowCount(), Primary!, dw_1, 1 ,Delete!)

If dw_2.Object.meen_guiemi[1] = 1 Or dw_2.Object.meen_guiemi[1] = 3 Then
	dw_2.Object.meen_guiemi[1] = 2
	w_main.SetMicroHelp("Anulando Registro...")
	If wf_actualiza_db(True) Then
		If Not iuo_Guia.of_generaanulaguia(dw_2.Object.clie_codigo[1], dw_2.Object.plde_codigo[1], dw_2.Object.meen_guisii[1], &
									dw_2.Object.meen_numero[1], dw_2.Object.tpmv_codigo[1], '', 2, 'Anulacion de Movimiento Envases.') Then
			MessageBox('Error', 'No se pudo cargar la tabla de Guias Nulas')
		End If		
								
		w_main.SetMicroHelp("Registro Anulado...")
		This.TriggerEvent("ue_nuevo")
		SetPointer(Arrow!)
	Else
		w_main.SetMicroHelp("Registro no se pudo Anular...")
	End If
Else
	If dw_2.DeleteRow(0) = 1 Then
		ib_borrar = False
		w_main.SetMicroHelp("Borrando Registro...")
		If wf_actualiza_db(True) Then
			w_main.SetMicroHelp("Registro Borrado...")
			This.TriggerEvent("ue_nuevo")
			SetPointer(Arrow!)
		Else
			w_main.SetMicroHelp("Registro no Borrado...")
		End If			
	Else
		ib_borrar = False
		MessageBox(This.Title,"No se puede borrar actual registro.")
	End If
End If
end event

type dw_1 from w_mant_encab_deta_csd`dw_1 within w_maed_movtoenvases
integer x = 50
integer y = 916
integer width = 3173
integer height = 1100
string title = "Detalle de Movimiento de Envases"
string dataobject = "dw_mues_movtoenvadeta_detalle"
end type

event type long dw_1::dwnkey(keycode key, unsignedlong keyflags);call super::dwnkey;il_fila = This.GetRow()

This.SetRedraw(False)

CHOOSE CASE key
	CASE KeyEnter!
		w_maed_movtoenvases.PostEvent("ue_modifica_detalle")

	CASE KeyRightArrow!, KeyLeftArrow!
		This.SetRedraw(True)
		RETURN -1
		
	CASE KeyDownArrow!, KeyUpArrow!, KeyPageUp!, KeyPageDown!, KeyEnd!, KeyHome!
		w_maed_movtoenvases.PostEvent("ue_seteafila")

END CHOOSE

This.SetRedraw(True)

RETURN 0
end event

event dw_1::losefocus;call super::losefocus;AcceptText()

This.SelectRow(0, False)

RETURN 0
end event

event dw_1::clicked;call super::clicked;IF Row > 0 THEN
	il_fila = Row
	This.SelectRow(0,False)
	This.SetRow(il_fila)
	This.SelectRow(il_fila,True)
END IF

RETURN 0
end event

type dw_2 from w_mant_encab_deta_csd`dw_2 within w_maed_movtoenvases
boolean visible = false
integer x = 32
integer y = 44
integer width = 3118
integer height = 712
string dataobject = "dw_mant_movtoenvaenca"
end type

event dw_2::itemchanged;Integer	li_Null
String	ls_Columna, ls_Nula

ls_Columna = dwo.Name
SetNull(li_Null)
SetNull(ls_Nula)

CHOOSE CASE ls_Columna
	CASE "clie_codigo"
		If NoExisteCliente(Integer(data)) Then
			This.SetItem(row,"clie_codigo",Integer(ls_Nula))
			Return 1
		End If
		istr_Mant.Argumento[16] = data
		
	CASE "meen_numero"
		If NOT ExisteMovimiento(gstr_ParamPlanta.CodigoPlanta, integer(istr_mant.argumento[2]), Integer(data)) Then
			This.SetItem(1,"meen_numero", li_Null)
			This.SetFocus()
			Return 1
		End If	
		
	CASE "plde_coorde"
		If Not iuo_PltaDestino.Existe(Integer(Data), True, sqlca) Then
			This.SetItem(1, ls_Columna, li_Null)
			
			Return 1
		ElseIf iuo_PltaDestino.Codigo = gstr_ParamPlanta.CodigoPlanta Then
			MessageBox("Atención", "No puede Despachar a la Planta de Origen.~r~r" + &
						"Ingrese o seleccione otra Planta de Destino.", Exclamation!, Ok!)
			
			Return 1
		End If
		
   CASE "tpmv_codigo"
		If NoExisteTipoMov(Integer(Data)) Then
			This.SetItem(1, ls_Columna, li_Null)
			Return 1
		Else
			istr_mant.argumento[2] = String(Integer(Data))
			istr_mant.argumento[4] = string(ii_sentido)
			istr_mant.argumento[5] = string(ii_solori)
			
			If Integer(istr_mant.argumento[5]) <> 2 Then
			
				dw_2.Object.plde_coorde.Protect				=	0
				dw_2.Object.plde_coorde.Color				=	0
				dw_2.Object.plde_coorde.BackGround.Color	=	RGB(255,255,255)
				
				dw_2.Object.prod_rut.Protect           				=  1
				
				dw_2.Object.prod_rut.Color  						=  RGB(255,255,255)
				dw_2.Object.prod_nombre.Color  					=  RGB(255,255,255)
				dw_2.Object.prod_rut.BackGround.Color  		=  553648127
				dw_2.Object.prod_nombre.BackGround.Color  	=  553648127
		   	Else
				dw_2.Object.plde_coorde.Protect				=	1
				dw_2.Object.plde_coorde.Color				=	RGB(255,255,255)
				dw_2.Object.plde_coorde.BackGround.Color	=	553648127
				
				dw_2.Object.prod_rut.Protect           				=  0
				dw_2.Object.prod_rut.Color  						=  0
				dw_2.Object.prod_nombre.Color  					=  0
				dw_2.Object.prod_rut.BackGround.Color  		=  RGB(255,255,255)
				dw_2.Object.prod_nombre.BackGround.Color  	=  RGB(255,255,255)
		   	End If

			If ii_tipodoc=2 Then 
				dw_2.Object.meen_guisii.Protect				=	1
				dw_2.Object.meen_guisii.Color					=	RGB(255,255,255)
				dw_2.Object.meen_guisii.BackGround.Color	=	553648127
			Else
				dw_2.Object.meen_guisii.Protect				=	0
				dw_2.Object.meen_guisii.Color					=	0
				dw_2.Object.meen_guisii.BackGround.Color	=	RGB(255,255,255)
			End If
		End If

	CASE "prod_rut"
		is_rutprod = F_verrut(data, True)
		If is_rutprod <> "" Then
			If ExisteProdRut(is_rutprod) AND ii_Cantidad > 1 Then
				SeleccionaProductor()
			Else
				dw_2.SetItem(1, "prod_codigo", il_Productor)
				dw_2.SetItem(1, "prod_rut", is_RutProductor)
				dw_2.SetItem(1, "prod_nombre", is_NombreProductor)
				istr_mant.argumento[6]	=	String(il_Productor)
				istr_mant.argumento[8]	=	is_NombreProductor
			End If
		Else
			This.SetItem(row, ls_Columna, String(li_Null))
			This.SetItem(row, "prod_codigo", li_Null)
			This.SetItem(row, "prod_nombre", String(li_Null))
			Return 1
		End If
	
	CASE "prod_codigo"
		If NoExisteCodigoProd(Long(Data)) Then
	  		This.SetItem(row, ls_Columna, li_Null)
	  	  	This.SetItem(row, "prod_rut", String(li_Null))
	     	This.SetItem(row, "prod_nombre", String(li_Null))
	     	Return 1
	  	Else
	     	This.Object.prod_nombre[row]  =	is_nomprod
			This.Object.prod_rut[row]	  =	is_rutprod
	  	End If	

	CASE "tran_codigo"
		If Not iuo_Transport.Existe(Integer(Data), True, sqlca) Then
			This.SetItem(1, ls_Columna, li_Null)
			
			Return 1
		End If
		
	CASE "cami_clasifi"
			istr_Mant.Argumento[6] = Data
			
	CASE "cami_patent"
		If istr_Mant.Argumento[6] <> "" Then
			If data <> "" AND Not iuo_Camion.Existe(Integer(istr_Mant.Argumento[6]), Data, True, sqlca) Then
				This.SetItem(1, ls_Columna, ls_Nula)			
				Return 1
			Else
				This.Object.cami_patcar[1]	=	iuo_Camion.PateCarro
				This.Object.meen_rutcho[1]	=	iuo_Camion.RutChofer
				This.Object.meen_chofer[1]	=	iuo_Camion.Chofer
			End If
		Else
			MessageBox("Atención","Falta Seleccionar ClasIficación de Camión",Exclamation!)
			This.SetItem(1, ls_Columna, ls_Nula)
			Return 1
		End If	
	
	CASE "meen_rutcho"
		is_rut = F_verrut(data, True)
		If is_rut = "" Then
			dw_2.SetItem(1, "meen_rutcho", ls_Nula)
			dw_2.SetItem(1, "meen_chofer", ls_Nula)
			Return 1
		Else
			If NoExisteChofer(is_rut) Then
				dw_2.SetItem(1, "meen_chofer", ls_Nula)
			Else	
				dw_2.SetItem(1, "meen_chofer",is_chofer)
			End If
		End If	

End CHOOSE

HabilitaIngreso(ls_Columna)
end event

event dw_2::buttonclicked;call super::buttonclicked;String	lb_Boton

lb_Boton = dwo.Name

Choose Case lb_Boton
	Case  "b_buscaproductor"
		buscaproductor()		

	Case  "buscacamion"
		If istr_Mant.Argumento[6] <> "" Then
			BuscaCamion()
			iuo_Camion.Existe(Integer(istr_Mant.Argumento[6]), This.Object.cami_patent[row], True, sqlca)
		Else
			MessageBox("Atención","Falta Seleccionar Clasificación de Camión",Exclamation!)
			Return 1
		End If
End Choose

end event

event dw_2::itemfocuschanged;call super::itemfocuschanged;IF is_rut <> "" THEN
	This.Object.meen_rutcho.Format = '@@@.@@@.@@@-@'
	IF dwo.Name <> "meen_rutcho" THEN
		This.SetItem(1, "meen_rutcho", is_rut)
	END IF
END IF

IF is_rutprod <> "" THEN
	This.Object.prod_rut.Format = '@@@.@@@.@@@-@'
	IF dwo.Name <> "prod_rut" THEN
		This.SetItem(1, "prod_rut", is_rutprod)
	END IF
END IF
	
end event

event dw_2::losefocus;call super::losefocus;IF is_rut <> "" THEN This.SetItem(1, "meen_rutcho", is_rut)

end event

type pb_nuevo from w_mant_encab_deta_csd`pb_nuevo within w_maed_movtoenvases
integer x = 3319
end type

type pb_eliminar from w_mant_encab_deta_csd`pb_eliminar within w_maed_movtoenvases
integer x = 3355
integer y = 576
end type

type pb_grabar from w_mant_encab_deta_csd`pb_grabar within w_maed_movtoenvases
integer x = 3319
end type

type pb_imprimir from w_mant_encab_deta_csd`pb_imprimir within w_maed_movtoenvases
integer x = 3319
end type

type pb_salir from w_mant_encab_deta_csd`pb_salir within w_maed_movtoenvases
integer x = 3319
end type

type pb_ins_det from w_mant_encab_deta_csd`pb_ins_det within w_maed_movtoenvases
integer textsize = -8
fontcharset fontcharset = ansi!
end type

type pb_eli_det from w_mant_encab_deta_csd`pb_eli_det within w_maed_movtoenvases
integer x = 3296
end type

type pb_buscar from w_mant_encab_deta_csd`pb_buscar within w_maed_movtoenvases
integer x = 3319
end type

type cb_guia from commandbutton within w_maed_movtoenvases
integer x = 3319
integer y = 1192
integer width = 302
integer height = 112
integer taborder = 80
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string text = "Guía SII"
end type

event clicked;Long		ll_Fila, ll_Fila_Busca
str_mant	lstr_mant

lstr_Mant.Argumento[1]	=	String(dw_2.Object.plde_codigo[1])
lstr_Mant.Argumento[2]	=	String(dw_2.Object.tpmv_codigo[1])
lstr_Mant.Argumento[3]	=	String(dw_2.Object.meen_numero[1])
lstr_Mant.Argumento[5]	=	String(dw_2.Object.clie_codigo[1])
lstr_Mant.Argumento[6]	=	'2'
		
OpenWithParm(w_emis_guia_despacho_envases, lstr_Mant)

lstr_Mant = Message.PowerObjectParm
If lstr_Mant.Respuesta = 1 Then Parent.TriggerEvent("ue_recuperadatos")


end event

type tab_1 from tab within w_maed_movtoenvases
integer x = 32
integer width = 3218
integer height = 868
integer taborder = 40
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 16777215
boolean raggedright = true
boolean focusonbuttondown = true
integer selectedtab = 1
tabpage_1 tabpage_1
tabpage_2 tabpage_2
end type

on tab_1.create
this.tabpage_1=create tabpage_1
this.tabpage_2=create tabpage_2
this.Control[]={this.tabpage_1,&
this.tabpage_2}
end on

on tab_1.destroy
destroy(this.tabpage_1)
destroy(this.tabpage_2)
end on

type tabpage_1 from userobject within tab_1
integer x = 18
integer y = 104
integer width = 3182
integer height = 748
long backcolor = 16777215
string text = "Encabezado"
long tabtextcolor = 33554432
long tabbackcolor = 16777215
long picturemaskcolor = 536870912
dw_encabezado dw_encabezado
end type

on tabpage_1.create
this.dw_encabezado=create dw_encabezado
this.Control[]={this.dw_encabezado}
end on

on tabpage_1.destroy
destroy(this.dw_encabezado)
end on

type dw_encabezado from uo_dw within tabpage_1
integer x = 18
integer y = 36
integer width = 3136
integer height = 700
integer taborder = 11
string dataobject = "dw_mant_movtoenvaenca"
boolean vscrollbar = false
boolean border = false
end type

event buttonclicked;call super::buttonclicked;String	lb_Boton

lb_Boton = dwo.Name

Choose Case lb_Boton
	Case  "b_buscaproductor"
		buscaproductor()		

	Case  "buscacamion"
		If istr_Mant.Argumento[6] <> "" Then
			BuscaCamion()
			iuo_Camion.Existe(Integer(istr_Mant.Argumento[6]), This.Object.cami_patent[row], True, sqlca)
		Else
			MessageBox("Atención","Falta Seleccionar Clasificación de Camión",Exclamation!)
			Return 1
		End If
End Choose

end event

event itemchanged;call super::itemchanged;String	ls_Columna, ls_Null

ls_Columna = dwo.Name
SetNull(ls_Null)

Choose Case ls_Columna
	Case "clie_codigo"
		If NoExisteCliente(Integer(data)) Then
			This.SetItem(row,"clie_codigo",Integer(ls_Null))
			Return 1
		End If
		istr_Mant.Argumento[16] = data
		
	Case "meen_numero"
		If NOT ExisteMovimiento(gstr_ParamPlanta.CodigoPlanta, integer(istr_mant.argumento[2]), Integer(data)) Then
			This.SetItem(Row,"meen_numero", Long(ls_Null))
			This.SetFocus()
			Return 1
		End If	
		
	Case "plde_coorde"
		If Not iuo_PltaDestino.Existe(Integer(Data), True, sqlca) Then
			This.SetItem(1, ls_Columna, Long(ls_Null))
			Return 1
		ElseIf iuo_PltaDestino.Codigo = gstr_ParamPlanta.CodigoPlanta Then
			MessageBox("Atención", "No puede Despachar a la Planta de Origen.~r~r" + "Ingrese o seleccione otra Planta de Destino.", Exclamation!, Ok!)
			Return 1
		End If
		
   Case "tpmv_codigo"
		If NoExisteTipoMov(Integer(Data)) Then
			This.SetItem(Row, ls_Columna,Long(ls_Null))
			Return 1
		Else
			istr_mant.argumento[2] = String(Integer(Data))
			istr_mant.argumento[4] = string(ii_sentido)
			istr_mant.argumento[5] = string(ii_solori)
			
			If ii_solori <> 2 Then
				This.Object.plde_coorde.Protect				=	0
				This.Object.plde_coorde.Color					=	0
				This.Object.plde_coorde.BackGround.Color	=	RGB(255,255,255)
				
				This.Object.prod_rut.Protect           				=  1
				
				This.Object.prod_rut.Color  							=  RGB(255,255,255)
				This.Object.prod_nombre.Color  					=  RGB(255,255,255)
				This.Object.prod_rut.BackGround.Color  			=  553648127
				This.Object.prod_nombre.BackGround.Color  	=  553648127
		   	Else
				This.Object.plde_coorde.Protect					=	1
				This.Object.plde_coorde.Color					=	RGB(255,255,255)
				This.Object.plde_coorde.BackGround.Color	=	553648127
				
				This.Object.prod_rut.Protect           				=  0
				This.Object.prod_rut.Color  							=  0
				This.Object.prod_nombre.Color  					=  0
				This.Object.prod_rut.BackGround.Color  			=  RGB(255,255,255)
				This.Object.prod_nombre.BackGround.Color  	=  RGB(255,255,255)
		   	End If

			If ii_tipodoc=2 Then 
				This.Object.meen_guisii.Protect				=	1
				This.Object.meen_guisii.Color					=	RGB(255,255,255)
				This.Object.meen_guisii.BackGround.Color	=	553648127
			Else
				This.Object.meen_guisii.Protect				=	0
				This.Object.meen_guisii.Color					=	0
				This.Object.meen_guisii.BackGround.Color	=	RGB(255,255,255)
			End If
		End If

	Case "prod_rut"
		is_rutprod = F_verrut(data, True)
		If is_rutprod <> "" Then
			If ExisteProdRut(is_rutprod) AND ii_Cantidad > 1 Then
				SeleccionaProductor()
			Else
				This.SetItem(Row, "prod_codigo", il_Productor)
				This.SetItem(Row, "prod_rut", is_RutProductor)
				This.SetItem(Row, "prod_nombre", is_NombreProductor)
				istr_mant.argumento[6]	=	String(il_Productor)
				istr_mant.argumento[8]	=	is_NombreProductor
			End If
		Else
			This.SetItem(row, ls_Columna, ls_Null)
			This.SetItem(row, "prod_codigo", Long(ls_Null))
			This.SetItem(row, "prod_nombre", ls_Null)
			Return 1
		End If
	
	Case "prod_codigo"
		If NoExisteCodigoProd(Long(Data)) Then
	  		This.SetItem(row, ls_Columna, Long(ls_Null))
	  	  	This.SetItem(row, "prod_rut", ls_Null)
	     	This.SetItem(row, "prod_nombre", ls_Null)
	     	Return 1
	  	Else
	     	This.Object.prod_nombre[row]  =	is_nomprod
			This.Object.prod_rut[row]	  	=	is_rutprod
	  	End If	

	Case "tran_codigo"
		If Not iuo_Transport.Existe(Integer(Data), True, sqlca) Then
			This.SetItem(Row, ls_Columna, Long(ls_Null))
			Return 1
		End If
		
	Case "cami_clasifi"
			istr_Mant.Argumento[6] = Data
			
	Case "cami_patent"
		If istr_Mant.Argumento[6] <> "" Then
			If data <> "" AND Not iuo_Camion.Existe(Integer(istr_Mant.Argumento[6]), Data, True, sqlca) Then
				This.SetItem(1, ls_Columna, ls_Null)			
				Return 1
			Else
				This.Object.cami_patcar[Row]	=	iuo_Camion.PateCarro
				This.Object.meen_rutcho[Row]	=	iuo_Camion.RutChofer
				This.Object.meen_chofer[Row]	=	iuo_Camion.Chofer
			End If
		Else
			MessageBox("Atención","Falta Seleccionar ClasIficación de Camión",Exclamation!)
			This.SetItem(1, ls_Columna, ls_Null)
			Return 1
		End If	
	
	Case "meen_rutcho"
		is_rut = F_verrut(data, True)
		If is_rut = "" Then
			This.SetItem(1, "meen_rutcho", ls_Null)
			This.SetItem(1, "meen_chofer", ls_Null)
			Return 1
		Else
			If NoExisteChofer(is_rut) Then
				This.SetItem(1, "meen_chofer", ls_Null)
			Else	
				This.SetItem(1, "meen_chofer",is_chofer)
			End If
		End If	

End Choose

HabilitaIngreso(ls_Columna)
end event

event itemfocuschanged;call super::itemfocuschanged;IF is_rut <> "" THEN
	This.Object.meen_rutcho.Format = '@@@.@@@.@@@-@'
	IF dwo.Name <> "meen_rutcho" THEN
		This.SetItem(1, "meen_rutcho", is_rut)
	END IF
END IF

IF is_rutprod <> "" THEN
	This.Object.prod_rut.Format = '@@@.@@@.@@@-@'
	IF dwo.Name <> "prod_rut" THEN
		This.SetItem(1, "prod_rut", is_rutprod)
	END IF
END IF
	
end event

event losefocus;call super::losefocus;IF is_rut <> "" THEN This.SetItem(1, "meen_rutcho", is_rut)

end event

type tabpage_2 from userobject within tab_1
integer x = 18
integer y = 104
integer width = 3182
integer height = 748
long backcolor = 16777215
string text = "Observacion"
long tabtextcolor = 33554432
long tabbackcolor = 16777215
long picturemaskcolor = 536870912
dw_observacion dw_observacion
end type

on tabpage_2.create
this.dw_observacion=create dw_observacion
this.Control[]={this.dw_observacion}
end on

on tabpage_2.destroy
destroy(this.dw_observacion)
end on

type dw_observacion from uo_dw within tabpage_2
integer x = 5
integer y = 32
integer width = 3118
integer height = 696
integer taborder = 11
string dataobject = "dw_mant_movtoenvaenca_observ"
boolean vscrollbar = false
boolean border = false
end type

