$PBExportHeader$w_infocondicionusdacondespachosusda.srw
$PBExportComments$Informe de Repalletizajes.
forward
global type w_infocondicionusdacondespachosusda from w_para_informes
end type
type st_4 from statictext within w_infocondicionusdacondespachosusda
end type
type st_1 from statictext within w_infocondicionusdacondespachosusda
end type
type dw_2 from datawindow within w_infocondicionusdacondespachosusda
end type
type st_6 from statictext within w_infocondicionusdacondespachosusda
end type
type dw_1 from datawindow within w_infocondicionusdacondespachosusda
end type
type em_desde from editmask within w_infocondicionusdacondespachosusda
end type
type em_hasta from editmask within w_infocondicionusdacondespachosusda
end type
type st_2 from statictext within w_infocondicionusdacondespachosusda
end type
type st_5 from statictext within w_infocondicionusdacondespachosusda
end type
type cbx_rotulo from checkbox within w_infocondicionusdacondespachosusda
end type
type cbx_todcliente from checkbox within w_infocondicionusdacondespachosusda
end type
type cbx_todplanta from checkbox within w_infocondicionusdacondespachosusda
end type
end forward

global type w_infocondicionusdacondespachosusda from w_para_informes
integer x = 14
integer y = 32
integer width = 2656
integer height = 1256
string title = "INFORME CONDICION Y DESPACHOS USDA"
boolean minbox = false
boolean maxbox = false
boolean resizable = false
windowtype windowtype = response!
string icon = "F:\Desarrollo\Producción\FrutaProcesada\Producc.ico"
event type integer ue_guardar ( )
st_4 st_4
st_1 st_1
dw_2 dw_2
st_6 st_6
dw_1 dw_1
em_desde em_desde
em_hasta em_hasta
st_2 st_2
st_5 st_5
cbx_rotulo cbx_rotulo
cbx_todcliente cbx_todcliente
cbx_todplanta cbx_todplanta
end type
global w_infocondicionusdacondespachosusda w_infocondicionusdacondespachosusda

type variables
str_busqueda istr_busq
str_mant istr_mant

DataWindowChild	idwc_tipopro, idwc_cliente, idwc_planta,idwc_especie, idwc_condicion

Long		il_sag
String	is_destino

uo_condicion    iuo_condicion      

end variables

on w_infocondicionusdacondespachosusda.create
int iCurrent
call super::create
this.st_4=create st_4
this.st_1=create st_1
this.dw_2=create dw_2
this.st_6=create st_6
this.dw_1=create dw_1
this.em_desde=create em_desde
this.em_hasta=create em_hasta
this.st_2=create st_2
this.st_5=create st_5
this.cbx_rotulo=create cbx_rotulo
this.cbx_todcliente=create cbx_todcliente
this.cbx_todplanta=create cbx_todplanta
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_4
this.Control[iCurrent+2]=this.st_1
this.Control[iCurrent+3]=this.dw_2
this.Control[iCurrent+4]=this.st_6
this.Control[iCurrent+5]=this.dw_1
this.Control[iCurrent+6]=this.em_desde
this.Control[iCurrent+7]=this.em_hasta
this.Control[iCurrent+8]=this.st_2
this.Control[iCurrent+9]=this.st_5
this.Control[iCurrent+10]=this.cbx_rotulo
this.Control[iCurrent+11]=this.cbx_todcliente
this.Control[iCurrent+12]=this.cbx_todplanta
end on

on w_infocondicionusdacondespachosusda.destroy
call super::destroy
destroy(this.st_4)
destroy(this.st_1)
destroy(this.dw_2)
destroy(this.st_6)
destroy(this.dw_1)
destroy(this.em_desde)
destroy(this.em_hasta)
destroy(this.st_2)
destroy(this.st_5)
destroy(this.cbx_rotulo)
destroy(this.cbx_todcliente)
destroy(this.cbx_todplanta)
end on

event open;call super::open;String	ls_Planta

SELECT	plde_nombre
	INTO	:ls_Planta
	FROM	dbo.plantadesp
	WHERE	plde_codigo	=	:gi_CodPlanta ;

dw_2.GetChild("clie_codigo", idwc_cliente)
idwc_cliente.SetTransObject(sqlca)
idwc_cliente.Retrieve(-1)
dw_2.InsertRow(0)
//dw_2.SetItem(1, "clie_codigo", gi_CodExport)

dw_1.GetChild("plde_codigo", idwc_planta)
idwc_planta.SetTransObject(sqlca)
idwc_planta.Retrieve(-1)
dw_1.InsertRow(0)
dw_1.SetItem(1, "plde_codigo", gi_CodPlanta)

istr_mant.argumento[1]	=	String(gi_codplanta)
istr_mant.argumento[2]	=	"2"//Condicion
istr_mant.argumento[3]	=	'-1'//String(gi_codexport)
istr_mant.argumento[5]	=	'0'//rotulados
istr_mant.argumento[6]  =  String(Today())//desde
istr_mant.argumento[7]  =  String(Today())//Hasta

em_hasta.Text = String(Today())
em_desde.Text = String(Today())



end event

type pb_excel from w_para_informes`pb_excel within w_infocondicionusdacondespachosusda
end type

type st_computador from w_para_informes`st_computador within w_infocondicionusdacondespachosusda
end type

type st_usuario from w_para_informes`st_usuario within w_infocondicionusdacondespachosusda
end type

type st_temporada from w_para_informes`st_temporada within w_infocondicionusdacondespachosusda
end type

type p_logo from w_para_informes`p_logo within w_infocondicionusdacondespachosusda
end type

type st_titulo from w_para_informes`st_titulo within w_infocondicionusdacondespachosusda
integer y = 280
integer width = 1902
string text = "Informe Condición y Despachos USDA"
end type

type pb_acepta from w_para_informes`pb_acepta within w_infocondicionusdacondespachosusda
string tag = "Imprimir Reporte"
integer x = 2322
integer taborder = 60
alignment htextalign = center!
end type

event pb_acepta::clicked;SetPointer(Arrow!)
Integer	fila,li_cbx_planta, Respuesta , li_rotulado
Long		li_planta,li_cliente

istr_info.titulo	= 'INFORME CONDICION Y DESPACHOS USDA'

OpenWithParm(vinf, istr_info)

vinf.dw_1.DataObject = "dw_infocondicionusdacondespachousda"

vinf.dw_1.SetTransObject(sqlca)

istr_mant.Argumento[2] = '2'

IF cbx_rotulo.Checked THEN
	li_rotulado = -1
ELSE
	li_rotulado = 0
END IF	

istr_mant.argumento[6] = em_desde.Text
istr_mant.argumento[7] = em_hasta.Text

fila	=	vinf.dw_1.Retrieve(Integer(istr_mant.Argumento[3]),Long(istr_mant.Argumento[1]), &
									Integer(istr_mant.Argumento[2]),Date(istr_mant.argumento[6]),&
									Date(istr_mant.argumento[7]),li_rotulado)

IF fila = -1 THEN
MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
				"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ELSEIF fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", &
				StopSign!, Ok!)
	ELSE
		F_Membrete(vinf.dw_1)
		//vinf.dw_1.Modify("planta.text = '" + istr_mant.Argumento[5] + "'")
		IF gs_Ambiente <> 'Windows' THEN
			F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
		END IF
END IF

SetPointer(Arrow!)				
end event

type pb_salir from w_para_informes`pb_salir within w_infocondicionusdacondespachosusda
string tag = "Salir [Cerrar Ventana Activa]"
integer x = 2318
integer y = 824
integer taborder = 80
string powertiptext = "Salir [Cerrar Ventana Activa]"
end type

type st_4 from statictext within w_infocondicionusdacondespachosusda
integer x = 251
integer y = 440
integer width = 1902
integer height = 580
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 16711680
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_1 from statictext within w_infocondicionusdacondespachosusda
integer x = 343
integer y = 580
integer width = 462
integer height = 76
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
boolean enabled = false
string text = "Planta"
boolean focusrectangle = false
end type

type dw_2 from datawindow within w_infocondicionusdacondespachosusda
integer x = 626
integer y = 472
integer width = 1161
integer height = 96
integer taborder = 10
boolean bringtotop = true
boolean enabled = false
string dataobject = "dddw_clientesprod"
boolean border = false
boolean livescroll = true
end type

event itemchanged;String	ls_Columna[]
Integer	li_planta	

IF ExisteCliente(Integer(data), ls_Columna[]) THEN
	istr_mant.argumento[3]	=	data
ELSE
	This.SetItem(1, "clie_codigo", gi_CodExport)
	
	RETURN 1
END IF
end event

event itemerror;RETURN 1
end event

type st_6 from statictext within w_infocondicionusdacondespachosusda
integer x = 343
integer y = 480
integer width = 233
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Cliente"
boolean focusrectangle = false
end type

type dw_1 from datawindow within w_infocondicionusdacondespachosusda
integer x = 626
integer y = 568
integer width = 969
integer height = 92
integer taborder = 30
boolean bringtotop = true
string dataobject = "dddw_plantadesp_tipo"
boolean border = false
boolean livescroll = true
end type

event itemchanged;String	ls_Columna[]

IF ExistePlanta(Integer(istr_mant.argumento[3]), Integer(data), ls_Columna[]) THEN
	istr_mant.argumento[1]	=	data
	
ELSE
	This.SetItem(1, "plde_codigo", gi_CodPlanta)
	
	RETURN 1
END IF
end event

event itemerror;RETURN 1
end event

type em_desde from editmask within w_infocondicionusdacondespachosusda
integer x = 626
integer y = 692
integer width = 402
integer height = 96
integer taborder = 50
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

type em_hasta from editmask within w_infocondicionusdacondespachosusda
integer x = 1641
integer y = 692
integer width = 402
integer height = 96
integer taborder = 60
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

type st_2 from statictext within w_infocondicionusdacondespachosusda
integer x = 343
integer y = 712
integer width = 210
integer height = 76
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
boolean enabled = false
string text = "Desde"
boolean focusrectangle = false
end type

type st_5 from statictext within w_infocondicionusdacondespachosusda
integer x = 1371
integer y = 712
integer width = 210
integer height = 76
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
boolean enabled = false
string text = "Hasta"
boolean focusrectangle = false
end type

type cbx_rotulo from checkbox within w_infocondicionusdacondespachosusda
integer x = 1001
integer y = 864
integer width = 402
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Rotulados"
end type

type cbx_todcliente from checkbox within w_infocondicionusdacondespachosusda
integer x = 1797
integer y = 480
integer width = 320
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Todos"
boolean checked = true
end type

event clicked;IF This.Checked THEN
	dw_2.Enabled = False
	dw_2.Reset()
	idwc_cliente.Retrieve()
	dw_2.InsertRow(0)
	Istr_mant.argumento[3] = '-1'
ELSE
	dw_2.Enabled = True
	dw_2.SetItem(1, "clie_codigo", gi_CodExport)
	Istr_mant.argumento[3] = String(gi_CodExport)
END IF	
end event

type cbx_todplanta from checkbox within w_infocondicionusdacondespachosusda
boolean visible = false
integer x = 1797
integer y = 572
integer width = 320
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
boolean enabled = false
string text = "Todos"
end type

event clicked;IF This.Checked THEN
	dw_1.Enabled = False
	dw_1.Reset()
	idwc_planta.Retrieve(1)
	dw_1.InsertRow(0)
	istr_mant.argumento[1] = '-1'
ELSE
	dw_1.SetItem(1, "plde_codigo", gi_CodPlanta)
	dw_1.Enabled = True
	istr_mant.argumento[1] = string(gi_CodPlanta)
END IF	
end event

