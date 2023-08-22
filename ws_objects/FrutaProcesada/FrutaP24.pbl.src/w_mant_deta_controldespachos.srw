$PBExportHeader$w_mant_deta_controldespachos.srw
forward
global type w_mant_deta_controldespachos from w_mant_detalle_csd
end type
end forward

global type w_mant_deta_controldespachos from w_mant_detalle_csd
integer width = 2459
integer height = 1312
string title = "CONTROL DESPACHOS"
end type
global w_mant_deta_controldespachos w_mant_deta_controldespachos

type variables
DataWindowChild	dw_cliente, dw_planta
end variables

forward prototypes
public function boolean duplicado (string columna, integer tipo)
public function datetime fechahora ()
public function boolean existedespacho (long numero)
end prototypes

public function boolean duplicado (string columna, integer tipo);Long		ll_fila, ll_numero
String	ls_codigo, ls_texto



CHOOSE CASE tipo
	CASE 1
		ls_codigo	= columna
		
END CHOOSE

ll_numero = Long(ls_codigo)

//ll_fila	= dw_1.Find( "clie_codigo = " + istr_mant.Argumento[1] + &
//						    " AND plde_codigo = " + istr_mant.Argumento[2] + &
//						    " AND defe_numero = " + String(ll_numero) + &
//							 " AND emba_codigo = '" + istr_mant.Argumento[3] + "'" + &
//							 " AND String(code_fechaa,'dd-mm-yyyy')	=	'"+String(istr_mant.argumento[5], "dd-mm-yyyy")+"'"+&
//							 " AND String(code_horaap,'00:00:00')	=	'"+String(istr_mant.argumento[6],"00:00:00")+"'",1,istr_mant.dw.RowCount())	
//							
							

//IF ll_fila > 0 and ll_fila <> il_filaAnc THEN
//	MessageBox("Error","Registro ya fue ingresado anteriormente",Information!, Ok!)
//	RETURN True
//ELSE
//	RETURN False
//END IF

RETURN False
end function

public function datetime fechahora ();Datetime ldt_FechaHora
Time		lt_Hora
Integer	li_Contador

IF sqlca.Dbms = "ODBC" THEN
	SELECT	Count(*), GetDate()
		INTO	:li_Contador, :ldt_FechaHora
		FROM	dbo.admasistemas;	
ELSE
	SELECT	Count(*), GetDate()
		INTO	:li_Contador, :ldt_FechaHora
		FROM	dbo.admasistemas;
END IF

RETURN ldt_FechaHora
end function

public function boolean existedespacho (long numero);Integer li_planta, li_cliente, li_count

li_cliente 	= Integer(istr_mant.argumento[1])
li_planta 	= Integer(istr_mant.argumento[2])

SELECT Count(*)
INTO	:li_count
FROM	dbo.DESPAFRIGOEN 
WHERE	plde_codigo =	:li_planta
AND	clie_codigo	=	:li_cliente
AND	defe_numero	=	:numero
AND 	Isnull(defe_estado,0) = 1;

IF sqlca.SQLCode = -1 THEN
		F_ErrorBaseDatos(sqlca,"Lectura de la Tabla DESPAFRIGOEN")
		RETURN True
ELSEIF li_count = 0 THEN
		MessageBox("Atención", "No existe Número Despacho Indicado o esta Abierto.~r~rIngrese otro Número.", &
						Exclamation!, Ok!)
		Return True						
ELSE						
		RETURN False
END IF

RETURN False
end function

on w_mant_deta_controldespachos.create
call super::create
end on

on w_mant_deta_controldespachos.destroy
call super::destroy
end on

event ue_recuperadatos;call super::ue_recuperadatos;ias_campo[4] = String(dw_1.GetItemNumber(il_fila,"defe_numero"))	
ias_campo[8] = String(dw_1.getItemString(il_fila,"code_motivo"))

IF istr_Mant.Agrega THEN
	dw_1.SetItem(il_fila, "clie_codigo", Integer(istr_mant.argumento[1]))
	dw_1.SetItem(il_fila, "plde_codigo", Integer(istr_mant.argumento[2]))
	dw_1.SetItem(il_fila, "usua_codigo", istr_mant.argumento[3])
	dw_1.SetItem(il_fila, "code_fechaa", Date(istr_mant.argumento[5]))
	dw_1.SetItem(il_fila, "code_horaap", Time(istr_mant.argumento[6]))
END IF

IF Not istr_mant.Agrega AND Not istr_mant.Borra THEN
	dw_1.Object.clie_codigo.Protect				=	1
	dw_1.Object.plde_codigo.Protect				=	1
	dw_1.Object.usua_codigo.Protect				=	1
	dw_1.Object.code_fechaa.Protect				=	1
	dw_1.Object.code_horaap.Protect				=	1
	dw_1.Object.defe_numero.Protect				=	1	

	dw_1.Object.clie_codigo.Color		=	RGB(255,255,255)
	dw_1.Object.plde_codigo.Color		=	RGB(255,255,255)
	dw_1.Object.defe_numero.Color	=	RGB(255,255,255)
	dw_1.Object.code_fechaa.Color	=	RGB(255,255,255)
	dw_1.Object.code_horaap.Color	=	RGB(255,255,255)
	dw_1.Object.defe_numero.Color	=	RGB(255,255,255)
	
	dw_1.Object.clie_codigo.BackGround.Color		=	553648127
	dw_1.Object.plde_codigo.BackGround.Color		=	553648127
	dw_1.Object.defe_numero.BackGround.Color	=	553648127
	dw_1.Object.code_fechaa.BackGround.Color	=	553648127
	dw_1.Object.code_horaap.BackGround.Color	=	553648127
	dw_1.Object.defe_numero.BackGround.Color	=	553648127
END IF

dw_1.SetFocus()
end event

event ue_deshace;call super::ue_deshace;IF UpperBound(ias_campo) > 0 THEN
	dw_1.SetItem(il_fila,"defe_numero",Long(ias_campo[4]))
	dw_1.SetItem(il_fila,"code_motivo",ias_campo[8])
END IF
end event

event ue_antesguardar;Integer	li_cont
String	ls_mensaje, ls_colu[]

IF Isnull(dw_1.GetItemNumber(il_fila, "clie_codigo")) OR dw_1.GetItemNumber(il_fila, "clie_codigo") = 0 THEN
	li_cont ++
	ls_mensaje 			= ls_mensaje + "~nCódigo de Cliente"
	ls_colu[li_cont]	= "clie_codigo"
END IF

IF Isnull(dw_1.GetItemNumber(il_fila, "plde_codigo")) OR dw_1.GetItemNumber(il_fila, "plde_codigo") = 0 THEN
	li_cont ++
	ls_mensaje			= ls_mensaje + "~nCódigo Planta"
	ls_colu[li_cont]	= "plde_codigo"
END IF

IF Isnull(dw_1.GetItemNumber(il_fila, "defe_numero")) OR dw_1.GetItemNumber(il_fila, "defe_numero") = 0 THEN
	li_cont ++
	ls_mensaje 			= ls_mensaje + "~nNúmero Despacho"
	ls_colu[li_cont]	= "defe_numero"
END IF

IF Isnull(dw_1.GetItemString(il_fila, "usua_codigo")) OR dw_1.GetItemString(il_fila, "usua_codigo") = "" THEN
	li_cont ++
	ls_mensaje 			= ls_mensaje + "~nUsuario"
	ls_colu[li_cont]	= "usua_codigo"
END IF

IF Isnull(dw_1.GetItemString(il_fila, "code_motivo")) OR dw_1.GetItemString(il_fila, "code_motivo") = "" THEN
	li_cont ++
	ls_mensaje			= ls_mensaje + "~nMotivo"
	ls_colu[li_cont]	= "code_motivo"
END IF

IF li_cont > 0 THEN
	MessageBox("Error de Consistencia", "Falta el ingreso de :" + ls_mensaje + ".", StopSign!, Ok!)
	dw_1.SetColumn(ls_colu[1])
	dw_1.SetFocus()
	Message.DoubleParm = -1
END IF
end event

event ue_nuevo;call super::ue_nuevo;Date ld_fecha
Time lt_hora

//ld_fecha = date(now()) 
//lt_hora = Time(fechahora())
//
//istr_mant.argumento[5] = String(ld_fecha)
//istr_mant.argumento[6] = String(lt_hora)

dw_1.SetItem(il_fila, "clie_codigo", Integer(istr_mant.argumento[1]))
dw_1.SetItem(il_fila, "plde_codigo", Integer(istr_mant.argumento[2]))
dw_1.SetItem(il_fila, "usua_codigo", String(istr_mant.argumento[3]))
dw_1.SetItem(il_fila, "code_fechaa", Date(istr_mant.argumento[5]))
dw_1.SetItem(il_fila, "code_horaap", Time(istr_mant.argumento[6]))



end event

event open;call super::open;x	= 100
y	= 450

PostEvent("ue_recuperadatos")

istr_mant = Message.PowerObjectParm

dw_1.SetTransObject(sqlca)

dw_1.GetChild("clie_codigo",dw_cliente)
dw_cliente.SetTransObject(sqlca)

IF dw_cliente.Retrieve(Integer(istr_mant.argumento[1])) = 0 THEN
	dw_cliente.InsertRow(0)
END IF

dw_1.GetChild("plde_codigo",dw_planta)
dw_planta.SetTransObject(sqlca)
IF dw_planta.Retrieve() = 0 THEN
	dw_planta.InsertRow(0)
END IF
end event

type pb_ultimo from w_mant_detalle_csd`pb_ultimo within w_mant_deta_controldespachos
unsignedlong __hwnd = 0
string tag = "Final"
boolean visible = true
accessiblerole accessiblerole = defaultrole!
integer x = 439
integer y = 0
integer width = 119
integer height = 92
integer taborder = 0
boolean dragauto = false
boolean bringtotop = false
integer transparency = 0
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean italic = false
boolean underline = false
boolean enabled = true
boolean cancel = false
boolean default = false
boolean flatstyle = false
boolean originalsize = false
string picturename = "\Desarrollo 17\Imagenes\Botones\Ultimo.png"
string disabledname = "\Desarrollo 17\Imagenes\Botones\Ultimo-bn.png"
alignment htextalign = left!
vtextalign vtextalign = bottom!
boolean map3dcolors = false
string powertiptext = "Final"
long textcolor = 0
long backcolor = 553648127
end type

type pb_siguiente from w_mant_detalle_csd`pb_siguiente within w_mant_deta_controldespachos
unsignedlong __hwnd = 0
string tag = "Siguiente"
boolean visible = true
accessiblerole accessiblerole = defaultrole!
integer x = 320
integer y = 0
integer width = 119
integer height = 92
integer taborder = 0
boolean dragauto = false
boolean bringtotop = false
integer transparency = 0
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean italic = false
boolean underline = false
boolean enabled = true
boolean cancel = false
boolean default = false
boolean flatstyle = false
boolean originalsize = false
string picturename = "\Desarrollo 17\Imagenes\Botones\Siguiente.png"
string disabledname = "\Desarrollo 17\Imagenes\Botones\Siguiente-bn.png"
alignment htextalign = left!
vtextalign vtextalign = bottom!
boolean map3dcolors = false
string powertiptext = "Siguiente"
long textcolor = 0
long backcolor = 553648127
end type

type pb_anterior from w_mant_detalle_csd`pb_anterior within w_mant_deta_controldespachos
unsignedlong __hwnd = 0
string tag = "Anterior"
boolean visible = true
accessiblerole accessiblerole = defaultrole!
integer x = 197
integer y = 0
integer width = 123
integer height = 92
integer taborder = 0
boolean dragauto = false
boolean bringtotop = false
integer transparency = 0
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean italic = false
boolean underline = false
boolean enabled = true
boolean cancel = false
boolean default = false
boolean flatstyle = false
boolean originalsize = false
string picturename = "\Desarrollo 17\Imagenes\Botones\Anterior.png"
string disabledname = "\Desarrollo 17\Imagenes\Botones\Anterior-bn.png"
alignment htextalign = left!
vtextalign vtextalign = bottom!
boolean map3dcolors = false
string powertiptext = "Anterior"
long textcolor = 0
long backcolor = 553648127
end type

type pb_primero from w_mant_detalle_csd`pb_primero within w_mant_deta_controldespachos
unsignedlong __hwnd = 0
string tag = "Inicio"
boolean visible = true
accessiblerole accessiblerole = defaultrole!
integer x = 78
integer y = 0
integer width = 119
integer height = 92
integer taborder = 0
boolean dragauto = false
boolean bringtotop = false
integer transparency = 0
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean italic = false
boolean underline = false
boolean enabled = true
boolean cancel = false
boolean default = false
boolean flatstyle = false
boolean originalsize = false
string picturename = "\Desarrollo 17\Imagenes\Botones\Primero.png"
string disabledname = "\Desarrollo 17\Imagenes\Botones\Primero-bn.png"
alignment htextalign = left!
vtextalign vtextalign = bottom!
boolean map3dcolors = false
string powertiptext = "Inicio"
long textcolor = 0
long backcolor = 553648127
end type

type pb_cancela from w_mant_detalle_csd`pb_cancela within w_mant_deta_controldespachos
unsignedlong __hwnd = 0
string tag = "Rechazar Acción"
boolean visible = true
accessiblerole accessiblerole = defaultrole!
integer x = 2126
integer y = 488
integer width = 302
integer height = 244
integer taborder = 30
boolean dragauto = false
boolean bringtotop = false
integer transparency = 0
integer textsize = -10
integer weight = 700
fontcharset fontcharset = defaultcharset!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean italic = false
boolean underline = false
boolean enabled = true
boolean cancel = false
boolean default = false
boolean flatstyle = false
boolean originalsize = false
string picturename = "\Desarrollo 17\Imagenes\Botones\Cancelar.png"
string disabledname = "\Desarrollo 17\Imagenes\Botones\Cancelar-bn.png"
alignment htextalign = left!
vtextalign vtextalign = bottom!
boolean map3dcolors = false
string powertiptext = "Rechazar Acción"
long textcolor = 0
long backcolor = 553648127
end type

type pb_acepta from w_mant_detalle_csd`pb_acepta within w_mant_deta_controldespachos
unsignedlong __hwnd = 0
string tag = "Aceptar Acción"
boolean visible = true
accessiblerole accessiblerole = defaultrole!
integer x = 2126
integer y = 272
integer width = 302
integer height = 244
integer taborder = 20
boolean dragauto = false
boolean bringtotop = false
integer transparency = 0
integer textsize = -10
integer weight = 700
fontcharset fontcharset = defaultcharset!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean italic = false
boolean underline = false
boolean enabled = true
boolean cancel = false
boolean default = true
boolean flatstyle = false
boolean originalsize = false
string picturename = "\Desarrollo 17\Imagenes\Botones\Aceptar.png"
string disabledname = "\Desarrollo 17\Imagenes\Botones\Aceptar-bn.png"
alignment htextalign = left!
vtextalign vtextalign = bottom!
boolean map3dcolors = false
string powertiptext = "Aceptar Acción"
long textcolor = 0
long backcolor = 553648127
end type

type pb_salir from w_mant_detalle_csd`pb_salir within w_mant_deta_controldespachos
unsignedlong __hwnd = 0
string tag = "Salir"
boolean visible = true
accessiblerole accessiblerole = defaultrole!
integer x = 2126
integer y = 704
integer width = 302
integer height = 244
integer taborder = 40
boolean dragauto = false
boolean bringtotop = false
integer transparency = 0
integer textsize = -10
integer weight = 700
fontcharset fontcharset = defaultcharset!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean italic = false
boolean underline = false
boolean enabled = true
boolean cancel = true
boolean default = false
boolean flatstyle = false
boolean originalsize = false
string picturename = "\Desarrollo 17\Imagenes\Botones\Apagar.png"
string disabledname = "\Desarrollo 17\Imagenes\Botones\Apagar-bn.png"
alignment htextalign = left!
vtextalign vtextalign = bottom!
boolean map3dcolors = false
string powertiptext = "Salir"
long textcolor = 0
long backcolor = 553648127
end type

type dw_1 from w_mant_detalle_csd`dw_1 within w_mant_deta_controldespachos
unsignedlong __hwnd = 0
boolean visible = true
accessiblerole accessiblerole = defaultrole!
integer x = 78
integer y = 108
integer width = 1929
integer height = 1068
integer taborder = 10
boolean dragauto = false
boolean bringtotop = false
integer transparency = 0
boolean enabled = true
boolean titlebar = false
string dataobject = "dw_mant_controldespachos"
richtexttoolbaractivation richtexttoolbaractivation = richtexttoolbaractivationonedit!
boolean controlmenu = false
boolean minbox = false
boolean maxbox = false
boolean hscrollbar = false
boolean vscrollbar = false
boolean resizable = false
boolean border = false
boolean hsplitscroll = false
boolean livescroll = false
borderstyle borderstyle = stylelowered!
boolean righttoleft = false
boolean ib_allow_updates = true
boolean ib_allow_inserts = true
long il_selected_row = 0
datetime grupofecha = DateTime(Date("1900-01-01"), Time("00:00:00.000000"))
end type

event dw_1::itemchanged;call super::itemchanged;Long		ll_fila
String	ls_columna, ls_null

ls_columna = GetColumnName()
SetNull(ls_null)

CHOOSE CASE ls_columna
	CASE "defe_numero"
		IF Duplicado(data, 1) THEN
			This.SetItem(il_fila, ls_columna, Integer(ls_null))
		   RETURN 1
		END IF
	
	IF Existedespacho(Long(data))  THEN
		This.SetItem(row, "defe_numero", Integer(ls_null))
		Return 1
	END IF
	
	
END CHOOSE
end event

event dw_1::itemerror;call super::itemerror;Return 1
end event

