$PBExportHeader$w_mant_mues_ctlcalagronomos.srw
$PBExportComments$Mantenedor de Agrónomos.
forward
global type w_mant_mues_ctlcalagronomos from w_mant_directo
end type
type st_4 from statictext within w_mant_mues_ctlcalagronomos
end type
type dw_zona from datawindow within w_mant_mues_ctlcalagronomos
end type
end forward

global type w_mant_mues_ctlcalagronomos from w_mant_directo
integer width = 2981
string title = "MAESTRO DE AGRONOMOS"
event ue_validapassword ( )
st_4 st_4
dw_zona dw_zona
end type
global w_mant_mues_ctlcalagronomos w_mant_mues_ctlcalagronomos

type variables
DataWindowChild	idwc_zona

Integer				ii_tipo, ii_orden

uo_zonas          iuo_zonas
end variables

forward prototypes
public function boolean duplicado (string columna, string valor)
end prototypes

event ue_validapassword();istr_mant.Argumento[1]	=	"Control de Calidad"
istr_mant.Argumento[2]	=	gstr_parlote.admispas

OpenWithParm(w_password, istr_mant)

istr_mant	=	Message.PowerObjectParm

IF istr_mant.Respuesta = 0 THEN Close(This)
end event

public function boolean duplicado (string columna, string valor);Long		ll_fila
Integer	li_zona, li_codigo

li_zona		=	Integer(istr_mant.argumento[1])
li_codigo	=	dw_1.Object.ccag_codigo[il_fila]

CHOOSE CASE columna
	CASE "ccag_codigo"
		li_codigo	=	Integer(valor)

END CHOOSE

ll_fila	= dw_1.Find("ccag_codigo = " + String(li_codigo), + &
							1, dw_1.RowCount())

IF ll_fila > 0 and ll_fila <> il_fila THEN
	MessageBox("Error","Registro ya fue ingresado anteriormente",Information!, Ok!)
	RETURN True
ELSE
	RETURN False
END IF

end function

event ue_recuperadatos;call super::ue_recuperadatos;Long		ll_fila, respuesta
Integer	li_Grupo
String	ls_Usuario

ls_Usuario	=	Upper(Gstr_Us.Nombre)

DO
	ll_fila	= dw_1.Retrieve(Integer(istr_mant.argumento[1]))

	IF ll_fila = -1 THEN
		respuesta = MessageBox(	"Error en Base de Datos", "No es posible conectar la Base de Datos.", &
										Information!, RetryCancel!)
	ELSEIF ll_fila > 0 THEN

		dw_1.SetFocus()
		il_fila					= 1
		pb_imprimir.Enabled	= True
		pb_insertar.Enabled	= True
		pb_eliminar.Enabled	= True
		pb_grabar.Enabled		= True
	ELSE
		pb_insertar.Enabled	= True
		pb_insertar.SetFocus()
	END IF
	
	li_Grupo = BuscaGrupo(ls_Usuario)
	
	IF	li_Grupo	=	2 OR li_Grupo	=	1 THEN 
		istr_mant.Solo_Consulta			=	False
	ELSE
		istr_mant.Solo_Consulta			=	True
	END IF
	
LOOP WHILE respuesta = 1

IF respuesta = 2 THEN Close(This)
end event

event open;call super::open;
dw_zona.GetChild("zona_codigo", idwc_zona)
idwc_zona.SetTransObject(sqlca)
idwc_zona.Retrieve()
dw_zona.InsertRow(0)

istr_mant.dw				= dw_1

PostEvent("ue_validapassword")

buscar			= "Código Agrónomo:Nccag_codigo,Nombre Agrónomo:Sccag_nombre,Abreviación:Sccag_abrevi"
ordenar			= "Código Agrónomo:ccag_codigo,Nombre Agrónomo:ccag_nombre,Abreviación:ccag_abrevi"

end event

on w_mant_mues_ctlcalagronomos.create
int iCurrent
call super::create
this.st_4=create st_4
this.dw_zona=create dw_zona
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_4
this.Control[iCurrent+2]=this.dw_zona
end on

on w_mant_mues_ctlcalagronomos.destroy
call super::destroy
destroy(this.st_4)
destroy(this.dw_zona)
end on

event ue_validaregistro();call super::ue_validaregistro;Integer	li_cont
String	ls_mensaje, ls_colu[]

IF Isnull(dw_1.Object.ccag_codigo[il_fila]) OR dw_1.Object.ccag_codigo[il_fila] = 0 THEN
	li_cont ++
	ls_mensaje 			= ls_mensaje + "~nCódigo de Agrónomo"
	ls_colu[li_cont]	= "ccag_codigo"
END IF

IF Isnull(dw_1.Object.ccag_nombre[il_fila]) OR dw_1.Object.ccag_nombre[il_fila] = "" THEN
	li_cont ++
	ls_mensaje 			= ls_mensaje + "~nDescripción del Agrónomo"
	ls_colu[li_cont]	= "ccag_nombre"
END IF

IF Isnull(dw_1.Object.ccag_abrevi[il_fila]) OR dw_1.Object.ccag_abrevi[il_fila] = "" THEN
	li_cont ++
	ls_mensaje 			= ls_mensaje + "~nAbreviación"
	ls_colu[li_cont]	= "ccag_abrevi"
END IF

IF li_cont > 0 THEN
	MessageBox("Error de Consistencia", "Falta el ingreso de :" + ls_mensaje + ".", StopSign!, Ok!)
	dw_1.SetColumn(ls_colu[1])
	dw_1.SetFocus()
	Message.DoubleParm = -1
END IF
end event

event ue_nuevo;call super::ue_nuevo;//dw_1.SetItem(il_Fila, "zona_codigo", Integer(istr_mant.argumento[1]))


end event

event ue_imprimir;SetPointer(HourGlass!)

Integer	li_zona
Long		fila
str_info	lstr_info

lstr_info.titulo	= "MAESTRO DE AGRONOMOS"
lstr_info.copias	= 1

OpenWithParm(vinf,lstr_info)

vinf.dw_1.DataObject = "dw_info_ctlcalagronomos"
vinf.dw_1.SetTransObject(sqlca)
li_zona	=	Integer(istr_mant.argumento[1])
fila = vinf.dw_1.Retrieve(li_zona)

IF fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ELSEIF fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", &
					StopSign!, Ok!)
ELSE
	F_Membrete(vinf.dw_1)
	vinf.dw_1.Sort()
	IF gs_Ambiente <> 'Windows' THEN F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
END IF

SetPointer(Arrow!)
end event

event ue_guardar();IF dw_1.AcceptText() = -1 THEN RETURN

SetPointer(HourGlass!)

w_main.SetMicroHelp("Grabando información...")

TriggerEvent("ue_antesguardar")

IF wf_actualiza_db() THEN
	w_main.SetMicroHelp("Información Grabada.")
	pb_imprimir.Enabled	= True
ELSE
	w_main.SetMicroHelp("No se puede Grabar información.")
	Message.DoubleParm = -1
	RETURN
END IF
end event

event ue_antesguardar();Long	ll_fila = 1

DO WHILE ll_fila <= dw_1.RowCount()
	IF dw_1.GetItemStatus(ll_fila, 0, Primary!) = New! THEN
		dw_1.DeleteRow(ll_fila)
	ELSE
		ll_fila ++
	END IF
LOOP

FOR ll_Fila	= 1 TO dw_1.RowCount()
	dw_1.Object.zona_codigo[ll_Fila] = Integer(istr_mant.argumento[1])
NEXT

end event

type st_encabe from w_mant_directo`st_encabe within w_mant_mues_ctlcalagronomos
integer x = 82
integer y = 64
integer width = 2341
integer height = 244
end type

type pb_nuevo from w_mant_directo`pb_nuevo within w_mant_mues_ctlcalagronomos
integer x = 2597
integer y = 408
integer taborder = 60
end type

event pb_nuevo::clicked;call super::clicked;dw_zona.Reset()

dw_zona.GetChild("zona_codigo", idwc_zona)
idwc_zona.SetTransObject(sqlca)
idwc_zona.Retrieve()
dw_zona.InsertRow(0)

dw_zona.Enabled	= True
dw_zona.SetFocus()

pb_lectura.Enabled	= False
pb_insertar.Enabled	= False
pb_eliminar.Enabled	= False
pb_grabar.Enabled		= False
pb_imprimir.Enabled	= False

il_fila					= 0




end event

type pb_lectura from w_mant_directo`pb_lectura within w_mant_mues_ctlcalagronomos
integer x = 2597
integer y = 136
integer taborder = 50
boolean enabled = false
end type

event pb_lectura::clicked;call super::clicked;dw_zona.Enabled	= False

end event

type pb_eliminar from w_mant_directo`pb_eliminar within w_mant_mues_ctlcalagronomos
integer x = 2597
integer y = 792
integer taborder = 90
end type

type pb_insertar from w_mant_directo`pb_insertar within w_mant_mues_ctlcalagronomos
integer x = 2597
integer y = 612
integer taborder = 80
end type

type pb_salir from w_mant_directo`pb_salir within w_mant_mues_ctlcalagronomos
integer x = 2597
integer y = 1536
integer taborder = 120
end type

type pb_imprimir from w_mant_directo`pb_imprimir within w_mant_mues_ctlcalagronomos
integer x = 2597
integer y = 1152
integer taborder = 110
end type

type pb_grabar from w_mant_directo`pb_grabar within w_mant_mues_ctlcalagronomos
integer x = 2597
integer y = 972
integer taborder = 100
end type

type dw_1 from w_mant_directo`dw_1 within w_mant_mues_ctlcalagronomos
integer x = 82
integer y = 388
integer width = 2341
integer height = 1292
integer taborder = 70
string dataobject = "dw_mues_ctlcalagronomos"
boolean hscrollbar = true
end type

event dw_1::itemchanged;call super::itemchanged;Long		ll_null

SetNull(ll_null)

CHOOSE CASE dwo.Name
	
	CASE "ccag_codigo"
		IF Duplicado(dwo.Name, data) THEN
			This.SetItem(il_fila, "ccag_codigo", ll_null)
			RETURN 1
		END IF

END CHOOSE
end event

event dw_1::rowfocuschanged;call super::rowfocuschanged;//IF CurrentRow > 0 AND il_fila > 0 THEN
//	ias_campo[1] = String(dw_1.Object.cctc_codigo[il_fila])
//	ias_campo[2] = dw_1.Object.cctc_nombres[il_fila]
//	ias_campo[3] = dw_1.Object.cctc_abrevi[il_fila]
//END IF

Integer li_Fila

li_Fila = dw_1.RowCount()

IF RowCount() < 1 OR GetRow() = 0 OR ib_borrar THEN
	ib_datos_ok = False
ELSE
	il_fila				=	GetRow()
	pb_grabar.Enabled	=	True
END IF

end event

event dw_1::itemfocuschanged;call super::itemfocuschanged;CHOOSE CASE dwo.Name

	CASE "ccag_abrevi"
			pb_grabar.Enabled	=	True

//	CASE "cctc_codigo"
//			TriggerEvent("ue_validaregistro")

END CHOOSE

end event

event dw_1::sqlpreview;//
end event

type st_4 from statictext within w_mant_mues_ctlcalagronomos
integer x = 594
integer y = 144
integer width = 210
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 33543637
boolean enabled = false
string text = "Zona"
boolean focusrectangle = false
end type

type dw_zona from datawindow within w_mant_mues_ctlcalagronomos
event itemchanged pbm_dwnitemchange
integer x = 841
integer y = 140
integer width = 850
integer height = 92
integer taborder = 20
boolean bringtotop = true
string dataobject = "dddw_zonas"
boolean border = false
boolean livescroll = true
end type

event itemchanged;Long ll_Null

SetNull(ll_Null)

iuo_zonas          = Create uo_zonas

IF Not iuo_zonas.existe(integer(data),True,sqlca) THEN
	this.SetItem(row, "zona_codigo", ll_Null)
   pb_lectura.Enabled		=	False
	RETURN 1 	
ELSE	
	istr_mant.argumento[1]	=	Data
	pb_lectura.Enabled		=	True
	pb_lectura.SetFocus()
	
END IF	
end event

event type long itemerror(long row, dwobject dwo, string data);RETURN 1 
end event

