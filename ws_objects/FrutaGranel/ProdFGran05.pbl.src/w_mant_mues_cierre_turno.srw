$PBExportHeader$w_mant_mues_cierre_turno.srw
forward
global type w_mant_mues_cierre_turno from w_mant_tabla
end type
type st_1 from statictext within w_mant_mues_cierre_turno
end type
type em_fecha from editmask within w_mant_mues_cierre_turno
end type
type em_turno from editmask within w_mant_mues_cierre_turno
end type
type dw_2 from datawindow within w_mant_mues_cierre_turno
end type
type st_2 from statictext within w_mant_mues_cierre_turno
end type
type st_3 from statictext within w_mant_mues_cierre_turno
end type
type st_4 from statictext within w_mant_mues_cierre_turno
end type
type sle_estado from singlelineedit within w_mant_mues_cierre_turno
end type
end forward

global type w_mant_mues_cierre_turno from w_mant_tabla
string tag = "w_mant_mues_cierre_turno"
integer width = 5221
string title = "Cierre de Turno"
windowstate windowstate = maximized!
st_1 st_1
em_fecha em_fecha
em_turno em_turno
dw_2 dw_2
st_2 st_2
st_3 st_3
st_4 st_4
sle_estado sle_estado
end type
global w_mant_mues_cierre_turno w_mant_mues_cierre_turno

type variables
datawindowchild 	      idwc_planta
end variables

on w_mant_mues_cierre_turno.create
int iCurrent
call super::create
this.st_1=create st_1
this.em_fecha=create em_fecha
this.em_turno=create em_turno
this.dw_2=create dw_2
this.st_2=create st_2
this.st_3=create st_3
this.st_4=create st_4
this.sle_estado=create sle_estado
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_1
this.Control[iCurrent+2]=this.em_fecha
this.Control[iCurrent+3]=this.em_turno
this.Control[iCurrent+4]=this.dw_2
this.Control[iCurrent+5]=this.st_2
this.Control[iCurrent+6]=this.st_3
this.Control[iCurrent+7]=this.st_4
this.Control[iCurrent+8]=this.sle_estado
end on

on w_mant_mues_cierre_turno.destroy
call super::destroy
destroy(this.st_1)
destroy(this.em_fecha)
destroy(this.em_turno)
destroy(this.dw_2)
destroy(this.st_2)
destroy(this.st_3)
destroy(this.st_4)
destroy(this.sle_estado)
end on

event open;call super::open;/*
   istr_mant.Argumento[1] = Planta
	istr_mant.Argumento[2] = Fecha de Vaciado
	istr_mant.Argumento[3] = Turno
	istr_mant.Argumento[4] = Estado a Buscar
*/

istr_Mant.Argumento[1]	=	String(gstr_ParamPlanta.CodigoPlanta)
istr_Mant.Argumento[2]	=	String(Today(),'dd/mm/yyyy')
istr_Mant.Argumento[3]	=	""
istr_Mant.Argumento[4]	=	Message.StringParm

if istr_Mant.Argumento[4] = 'V' then
	pb_imprimir.visible = true
else
	pb_imprimir.visible = false
end if

dw_2.GetChild("plde_codigo", idwc_planta)
idwc_planta.SetTransObject(sqlca)
idwc_planta.Retrieve(gi_codexport)
dw_2.SetTransObject(sqlca)
dw_2.insertrow(0)

dw_2.Object.plde_codigo[1]	=	gstr_ParamPlanta.CodigoPlanta
dw_2.Object.plde_codigo.Protect=1
dw_2.Object.plde_codigo.Color = RGB(255,255,255)
dw_2.Object.plde_codigo.BackGround.Color = 553648127

em_fecha.text	=	String(Today(),'dd/mm/yyyy')

IF istr_mant.Argumento[4] = "V" THEN
   this.Title  = "Cierre de Turno"
	sle_estado.text	=	"Vigente"
ELSE
	this.Title  = "Anula Cierre de Turno"
	sle_estado.Text	=	"Cerrado"	
END IF	

dw_1.SetTransObject(sqlca)
end event

event ue_recuperadatos;call super::ue_recuperadatos;Long		ll_fila_d, ll_fila_e, respuesta, ll_row
Date		ld_fecha

ld_Fecha	=	Date(Mid(istr_Mant.Argumento[2], 1, 10))

DO
	dw_1.SetRedraw(False)
	
	ll_fila_e	=	dw_1.Retrieve(Integer(istr_mant.Argumento[1]), &
										  ld_fecha, &
										  Integer(istr_mant.Argumento[3]), &
										  istr_Mant.Argumento[4])
	
	IF ll_fila_e = -1 THEN
		respuesta = MessageBox(	"Error en Base de Datos", "No es posible conectar la Base de Datos.", &
										Information!, RetryCancel!)
	ELSE
		pb_imprimir.Enabled	=	True
		dw_1.SetRedraw(True)
	END IF	
LOOP WHILE respuesta = 1

IF respuesta = 2 THEN Close(This)


end event

event ue_antesguardar();call super::ue_antesguardar;Long ll_row

FOR ll_row=1 TO dw_1.RowCount()
	IF istr_mant.Argumento[4] = "V" THEN
		dw_1.Object.opve_estado[ll_row]='C'
	ELSE
		dw_1.Object.opve_estado[ll_row]='V'
	END IF	
NEXT

IF istr_mant.Argumento[4] = "V" THEN
	sle_estado.Text	=	"Cerrado"
ELSE
	sle_estado.Text	=	"Vigente"	
END IF	

end event

event ue_imprimir;SetPointer(HourGlass!)

Long		fila
Date 		ld_fecha

ld_Fecha	=	Date(Mid(istr_Mant.Argumento[2], 1, 10))

IF istr_mant.Argumento[4] = "V" THEN
	istr_info.titulo	= "CIERRE DE TURNO"
ELSE
	istr_info.titulo	= "ANULACION CIERRE DE TURNO"
END IF	
istr_info.copias	= 1

OpenWithParm(vinf, istr_info)
vinf.dw_1.DataObject = "dw_info_cierre_turno"
vinf.dw_1.SetTransObject(sqlca)

IF istr_mant.Argumento[4] = "V" THEN
	fila = vinf.dw_1.Retrieve(Integer(istr_mant.Argumento[1]), ld_fecha, &
									  Integer(istr_mant.Argumento[3]), "C")
ELSE
	fila = vinf.dw_1.Retrieve(Integer(istr_mant.Argumento[1]), ld_fecha, &
									  Integer(istr_mant.Argumento[3]), "V")
END IF	

IF fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ELSEIF fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", StopSign!, Ok!)
ELSE
	F_Membrete(vinf.dw_1)
	IF gs_Ambiente <> 'Windows' THEN F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
END IF

SetPointer(Arrow!)
end event

type dw_1 from w_mant_tabla`dw_1 within w_mant_mues_cierre_turno
integer x = 41
integer y = 476
integer width = 4690
integer height = 1360
integer taborder = 40
string dataobject = "dw_mant_mues_cierre_turno"
boolean hscrollbar = true
end type

event dw_1::clicked;//
end event

event dw_1::doubleclicked;//
end event

event dw_1::getfocus;//
end event

event dw_1::itemfocuschanged;//
end event

event dw_1::rowfocuschanged;//
end event

type st_encabe from w_mant_tabla`st_encabe within w_mant_mues_cierre_turno
integer x = 41
integer y = 56
integer width = 4690
integer height = 388
end type

type pb_lectura from w_mant_tabla`pb_lectura within w_mant_mues_cierre_turno
integer x = 4754
integer y = 116
integer taborder = 30
end type

event pb_lectura::clicked;
istr_Mant.Argumento[2]	=	string(Date(em_fecha.text),'dd/mm/yyyy')
istr_Mant.Argumento[3]	=	String(em_turno.text)

IF istr_mant.Argumento[3]<>"" THEN

 em_fecha.Enabled=False
 em_turno.Enabled=False

 Parent.TriggerEvent("ue_recuperadatos")
 
END IF 
end event

type pb_nuevo from w_mant_tabla`pb_nuevo within w_mant_mues_cierre_turno
integer x = 4754
integer y = 412
integer taborder = 50
end type

event pb_nuevo::clicked;call super::clicked;


em_fecha.Enabled		=	TRUE
em_turno.Enabled		=	TRUE

pb_imprimir.Enabled  =	False
pb_grabar.Enabled		=	False

IF istr_mant.Argumento[4] = "V" THEN
	sle_estado.Text	=	"Vigente"
ELSE
	sle_estado.Text	=	"Cerrado"
END IF	
end event

type pb_insertar from w_mant_tabla`pb_insertar within w_mant_mues_cierre_turno
boolean visible = false
integer x = 2885
integer y = 628
integer taborder = 60
end type

type pb_eliminar from w_mant_tabla`pb_eliminar within w_mant_mues_cierre_turno
boolean visible = false
integer x = 2885
integer y = 808
integer taborder = 70
end type

type pb_grabar from w_mant_tabla`pb_grabar within w_mant_mues_cierre_turno
integer x = 4754
integer y = 952
integer taborder = 80
end type

type pb_imprimir from w_mant_tabla`pb_imprimir within w_mant_mues_cierre_turno
integer x = 4754
integer taborder = 90
end type

type pb_salir from w_mant_tabla`pb_salir within w_mant_mues_cierre_turno
integer x = 4754
integer y = 1516
integer taborder = 100
end type

type st_1 from statictext within w_mant_mues_cierre_turno
integer x = 1257
integer y = 152
integer width = 233
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
string text = "Planta"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type em_fecha from editmask within w_mant_mues_cierre_turno
string tag = "Fecha Vaciado"
integer x = 1586
integer y = 272
integer width = 416
integer height = 92
integer taborder = 10
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
string text = "none"
alignment alignment = center!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
end type

type em_turno from editmask within w_mant_mues_cierre_turno
string tag = "Turno"
integer x = 2391
integer y = 272
integer width = 155
integer height = 92
integer taborder = 20
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
string text = "none"
alignment alignment = center!
borderstyle borderstyle = stylelowered!
string mask = "#"
end type

type dw_2 from datawindow within w_mant_mues_cierre_turno
integer x = 1591
integer y = 120
integer width = 896
integer height = 96
boolean bringtotop = true
string title = "none"
string dataobject = "dddw_planta"
boolean border = false
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type st_2 from statictext within w_mant_mues_cierre_turno
integer x = 1253
integer y = 280
integer width = 325
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
string text = "F. Vaciado"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_3 from statictext within w_mant_mues_cierre_turno
integer x = 2853
integer y = 288
integer width = 224
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
string text = "Estado"
boolean focusrectangle = false
end type

type st_4 from statictext within w_mant_mues_cierre_turno
integer x = 2181
integer y = 280
integer width = 192
integer height = 72
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
string text = "Turno"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type sle_estado from singlelineedit within w_mant_mues_cierre_turno
integer x = 3095
integer y = 272
integer width = 535
integer height = 92
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 16711680
boolean displayonly = true
borderstyle borderstyle = stylelowered!
end type

