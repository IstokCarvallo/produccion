$PBExportHeader$w_mant_mues_cierre_turno_comer.srw
forward
global type w_mant_mues_cierre_turno_comer from w_mant_tabla
end type
type st_1 from statictext within w_mant_mues_cierre_turno_comer
end type
type em_fecha from editmask within w_mant_mues_cierre_turno_comer
end type
type em_turno from editmask within w_mant_mues_cierre_turno_comer
end type
type dw_2 from datawindow within w_mant_mues_cierre_turno_comer
end type
type st_2 from statictext within w_mant_mues_cierre_turno_comer
end type
type st_3 from statictext within w_mant_mues_cierre_turno_comer
end type
type st_4 from statictext within w_mant_mues_cierre_turno_comer
end type
type sle_estado from singlelineedit within w_mant_mues_cierre_turno_comer
end type
end forward

global type w_mant_mues_cierre_turno_comer from w_mant_tabla
string tag = "w_mant_mues_cierre_turno"
integer width = 3218
string title = "Cierre de Turno"
st_1 st_1
em_fecha em_fecha
em_turno em_turno
dw_2 dw_2
st_2 st_2
st_3 st_3
st_4 st_4
sle_estado sle_estado
end type
global w_mant_mues_cierre_turno_comer w_mant_mues_cierre_turno_comer

type variables
datawindowchild 	      idwc_planta
end variables

on w_mant_mues_cierre_turno_comer.create
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

on w_mant_mues_cierre_turno_comer.destroy
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

dw_2.GetChild("plde_codigo", idwc_planta)
idwc_planta.SetTransObject(sqlca)
idwc_planta.Retrieve()
dw_2.SetTransObject(sqlca)
dw_2.insertrow(0)

dw_2.Object.plde_codigo[1]	=	integer(istr_mant.argumento[1])
dw_2.Object.plde_codigo.Protect=1
dw_2.Object.plde_codigo.BackGround.Color = RGB(192,192,192)

em_fecha.text	=	String(Today(),'dd/mm/yyyy')

dw_1.SetTransObject(sqlca)

IF istr_mant.Argumento[4] = "V" THEN
   this.Title  = "Cierre de Turno"
	sle_estado.text	=	"Vigente"
ELSE
	this.Title  = "Anula Cierre de Turno"
	sle_estado.Text	=	"Cerrado"	
END IF	


end event

event ue_recuperadatos();call super::ue_recuperadatos;Long		ll_fila_d, ll_fila_e, respuesta, ll_row
Date		ldt_fecha

ldt_Fecha	=	Date(istr_Mant.Argumento[2])


DO
	dw_1.SetRedraw(False)
	
	ll_fila_e	=	dw_1.Retrieve(Integer(istr_mant.Argumento[1]), &
										  ldt_fecha, &
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

event ue_imprimir();SetPointer(HourGlass!)

Long		fila
Date		ldt_fecha

ldt_Fecha	=	Date(istr_Mant.Argumento[2])

IF istr_mant.Argumento[4] = "V" THEN
	istr_info.titulo	= "CIERRE DE TURNO"
ELSE
	istr_info.titulo	= "ANULACION CIERRE DE TURNO"
END IF	

istr_info.copias	= 1

OpenWithParm(vinf, istr_info)

vinf.dw_1.DataObject = "dw_info_cierre_turno_comer"

vinf.dw_1.SetTransObject(sqlca)

IF istr_mant.Argumento[4] = "V" THEN
	fila = vinf.dw_1.Retrieve(Integer(istr_mant.Argumento[1]), &
									  ldt_fecha, &
									  Integer(istr_mant.Argumento[3]), &
									  "C")
ELSE
	fila = vinf.dw_1.Retrieve(Integer(istr_mant.Argumento[1]), &
									  ldt_fecha, &
									  Integer(istr_mant.Argumento[3]), &
									  "V")
END IF

IF fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ELSEIF fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", &
					StopSign!, Ok!)
ELSE
	F_Membrete(vinf.dw_1)
	vinf.dw_1.Modify('DataWindow.Print.Preview = Yes')
	vinf.dw_1.Modify('DataWindow.Print.Preview.Zoom = 100')

	vinf.Visible	= True
	vinf.Enabled	= True
END IF

SetPointer(Arrow!)
end event

type dw_1 from w_mant_tabla`dw_1 within w_mant_mues_cierre_turno_comer
integer x = 82
integer y = 540
integer width = 2615
integer height = 1196
integer taborder = 50
string dataobject = "dw_mant_mues_cierre_turno_comer"
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

type st_encabe from w_mant_tabla`st_encabe within w_mant_mues_cierre_turno_comer
integer width = 2633
integer height = 388
end type

type pb_lectura from w_mant_tabla`pb_lectura within w_mant_mues_cierre_turno_comer
integer x = 2889
integer y = 152
integer taborder = 40
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

type pb_nuevo from w_mant_tabla`pb_nuevo within w_mant_mues_cierre_turno_comer
integer x = 2885
integer y = 448
integer taborder = 60
end type

event pb_nuevo::clicked;call super::clicked;


em_fecha.Enabled		=	TRUE
em_turno.Enabled		=	TRUE

pb_imprimir.Enabled  =	False
pb_grabar.Enabled		=	False

sle_estado.text = ""
end event

type pb_insertar from w_mant_tabla`pb_insertar within w_mant_mues_cierre_turno_comer
boolean visible = false
integer x = 2885
integer y = 628
integer taborder = 70
end type

type pb_eliminar from w_mant_tabla`pb_eliminar within w_mant_mues_cierre_turno_comer
boolean visible = false
integer x = 2885
integer y = 808
integer taborder = 80
end type

type pb_grabar from w_mant_tabla`pb_grabar within w_mant_mues_cierre_turno_comer
integer x = 2885
integer y = 988
integer taborder = 90
end type

type pb_imprimir from w_mant_tabla`pb_imprimir within w_mant_mues_cierre_turno_comer
integer x = 2885
integer y = 1168
integer taborder = 100
end type

type pb_salir from w_mant_tabla`pb_salir within w_mant_mues_cierre_turno_comer
integer x = 2885
integer y = 1552
integer taborder = 110
end type

type st_1 from statictext within w_mant_mues_cierre_turno_comer
integer x = 247
integer y = 164
integer width = 233
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
string text = "Planta"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type em_fecha from editmask within w_mant_mues_cierre_turno_comer
string tag = "Fecha Vaciado"
integer x = 576
integer y = 284
integer width = 343
integer height = 92
integer taborder = 10
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
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
end type

type em_turno from editmask within w_mant_mues_cierre_turno_comer
string tag = "Turno"
integer x = 1381
integer y = 284
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

type dw_2 from datawindow within w_mant_mues_cierre_turno_comer
integer x = 581
integer y = 132
integer width = 896
integer height = 96
boolean bringtotop = true
string title = "none"
string dataobject = "dddw_planta"
boolean border = false
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type st_2 from statictext within w_mant_mues_cierre_turno_comer
integer x = 242
integer y = 292
integer width = 325
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
string text = "F. Vaciado"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_3 from statictext within w_mant_mues_cierre_turno_comer
integer x = 1842
integer y = 300
integer width = 224
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
string text = "Estado"
boolean focusrectangle = false
end type

type st_4 from statictext within w_mant_mues_cierre_turno_comer
integer x = 1170
integer y = 292
integer width = 192
integer height = 72
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
string text = "Turno"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type sle_estado from singlelineedit within w_mant_mues_cierre_turno_comer
integer x = 2103
integer y = 284
integer width = 535
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
boolean enabled = false
borderstyle borderstyle = stylelowered!
end type

