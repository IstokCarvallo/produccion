$PBExportHeader$w_info_cambioaltura.srw
$PBExportComments$Informe de Revisión Repalletizajes.
forward
global type w_info_cambioaltura from w_para_informes
end type
type st_4 from statictext within w_info_cambioaltura
end type
type st_1 from statictext within w_info_cambioaltura
end type
type st_5 from statictext within w_info_cambioaltura
end type
type st_2 from statictext within w_info_cambioaltura
end type
type em_numero from editmask within w_info_cambioaltura
end type
type dw_2 from datawindow within w_info_cambioaltura
end type
type st_6 from statictext within w_info_cambioaltura
end type
type dw_1 from datawindow within w_info_cambioaltura
end type
type cb_buscarepa from commandbutton within w_info_cambioaltura
end type
type st_7 from statictext within w_info_cambioaltura
end type
type rb_1 from radiobutton within w_info_cambioaltura
end type
type rb_2 from radiobutton within w_info_cambioaltura
end type
end forward

global type w_info_cambioaltura from w_para_informes
integer x = 14
integer y = 32
integer width = 2770
integer height = 1468
string title = "CAMBIO DE PRODUCTOR / CALIBRE"
boolean minbox = false
boolean maxbox = false
boolean resizable = false
windowtype windowtype = response!
string icon = "F:\Desarrollo\Producción\FrutaProcesada\Producc.ico"
st_4 st_4
st_1 st_1
st_5 st_5
st_2 st_2
em_numero em_numero
dw_2 dw_2
st_6 st_6
dw_1 dw_1
cb_buscarepa cb_buscarepa
st_7 st_7
rb_1 rb_1
rb_2 rb_2
end type
global w_info_cambioaltura w_info_cambioaltura

type variables
str_busqueda istr_busq
str_mant istr_mant

DataWindowChild	idwc_tipopro, idwc_cliente, idwc_planta,idwc_especie
end variables

on w_info_cambioaltura.create
int iCurrent
call super::create
this.st_4=create st_4
this.st_1=create st_1
this.st_5=create st_5
this.st_2=create st_2
this.em_numero=create em_numero
this.dw_2=create dw_2
this.st_6=create st_6
this.dw_1=create dw_1
this.cb_buscarepa=create cb_buscarepa
this.st_7=create st_7
this.rb_1=create rb_1
this.rb_2=create rb_2
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_4
this.Control[iCurrent+2]=this.st_1
this.Control[iCurrent+3]=this.st_5
this.Control[iCurrent+4]=this.st_2
this.Control[iCurrent+5]=this.em_numero
this.Control[iCurrent+6]=this.dw_2
this.Control[iCurrent+7]=this.st_6
this.Control[iCurrent+8]=this.dw_1
this.Control[iCurrent+9]=this.cb_buscarepa
this.Control[iCurrent+10]=this.st_7
this.Control[iCurrent+11]=this.rb_1
this.Control[iCurrent+12]=this.rb_2
end on

on w_info_cambioaltura.destroy
call super::destroy
destroy(this.st_4)
destroy(this.st_1)
destroy(this.st_5)
destroy(this.st_2)
destroy(this.em_numero)
destroy(this.dw_2)
destroy(this.st_6)
destroy(this.dw_1)
destroy(this.cb_buscarepa)
destroy(this.st_7)
destroy(this.rb_1)
destroy(this.rb_2)
end on

event open;call super::open;String	ls_Planta, ls_cliente

SELECT	plde_nombre
	INTO	:ls_Planta
	FROM	dba.plantadesp
	WHERE	plde_codigo	=	:gi_CodPlanta ;
	
SELECT	clie_nombre
	INTO	:ls_cliente
	FROM	dba.clientesprod
	WHERE	clie_codigo	=	:gi_CodExport;

dw_2.GetChild("clie_codigo", idwc_cliente)
idwc_cliente.SetTransObject(SQLCA)
idwc_cliente.Retrieve()
dw_2.InsertRow(0)
dw_2.SetItem(1, "clie_codigo", gi_CodExport)

dw_1.GetChild("plde_codigo", idwc_planta)
idwc_planta.SetTransObject(sqlca)
idwc_planta.Retrieve(1)
dw_1.InsertRow(0)
dw_1.SetItem(1, "plde_codigo", gi_CodPlanta)

istr_mant.argumento[1]	=	String(gi_codplanta)
istr_mant.argumento[2]	=	""
istr_mant.argumento[3]	=	String(gi_codexport)
istr_mant.argumento[4]	=	String(gi_CodEspecie)
istr_mant.argumento[5]	=	ls_Planta
istr_mant.argumento[8]	=	ls_cliente
end event

type pb_excel from w_para_informes`pb_excel within w_info_cambioaltura
end type

type st_computador from w_para_informes`st_computador within w_info_cambioaltura
end type

type st_usuario from w_para_informes`st_usuario within w_info_cambioaltura
end type

type st_temporada from w_para_informes`st_temporada within w_info_cambioaltura
end type

type p_logo from w_para_informes`p_logo within w_info_cambioaltura
end type

type st_titulo from w_para_informes`st_titulo within w_info_cambioaltura
integer width = 1902
string text = "Informe de Cambio de Productor / Calibre"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_cambioaltura
integer x = 2331
integer y = 664
integer taborder = 40
end type

event pb_acepta::clicked;SetPointer(Arrow!)
Integer	fila,li_cbx_planta,li_especie,li_numero
Long		li_planta,li_cliente
String	ls_guia, ls_Planta

istr_info.titulo	= 'INFORME DE CAMBIO DE PRODUCTOR / CALIBRE'

OpenWithParm(vinf, istr_info)

IF rb_1.Checked THEN
	vinf.dw_1.DataObject = "dw_info_cambioaltura"
ELSE
	vinf.dw_1.DataObject = "dw_info_cambioaltura_cali"
END IF

ls_Planta	=	istr_mant.Argumento[5]
ls_guia		=	String(Long(istr_mant.Argumento[2]),'00000000')

vinf.dw_1.SetTransObject(sqlca)

fila	=	vinf.dw_1.Retrieve(Integer(istr_mant.Argumento[1]), &
									Long(istr_mant.Argumento[2]), &
									Integer(istr_mant.Argumento[3]))

IF fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
			   	"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)

ELSEIF fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", &
	             StopSign!, Ok!)

ELSE
	F_Membrete(vinf.dw_1)
	
	vinf.dw_1.Modify("planta.text = '" + ls_Planta + "'")
	vinf.dw_1.Modify("Fecha.text = '" + istr_mant.Argumento[6] + "'")
	vinf.dw_1.Modify("Observaciones.text = '" + istr_mant.Argumento[7] + "'")
	vinf.dw_1.Modify("nroguia.text = '" + ls_guia + "'")
	vinf.dw_1.Modify("cliente.text = '" + istr_mant.Argumento[8] + "'")
	
	IF gs_Ambiente <> 'Windows' THEN
		F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
	END IF
END IF

SetPointer(Arrow!)				
end event

type pb_salir from w_para_informes`pb_salir within w_info_cambioaltura
integer x = 2331
integer y = 984
integer taborder = 50
end type

type st_4 from statictext within w_info_cambioaltura
integer x = 251
integer y = 440
integer width = 1902
integer height = 340
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

type st_1 from statictext within w_info_cambioaltura
integer x = 347
integer y = 624
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

type st_5 from statictext within w_info_cambioaltura
integer x = 251
integer y = 780
integer width = 1902
integer height = 212
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

type st_2 from statictext within w_info_cambioaltura
integer x = 357
integer y = 856
integer width = 517
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
string text = "N° Proceso"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type em_numero from editmask within w_info_cambioaltura
integer x = 855
integer y = 844
integer width = 393
integer height = 92
integer taborder = 30
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
long backcolor = 16777215
alignment alignment = center!
borderstyle borderstyle = stylelowered!
string mask = "#####"
end type

event modified;Integer	li_planta, li_cliente
Long		ll_numero
String	ls_obser
Date		ld_fecha

li_cliente	=	Integer(istr_mant.argumento[3])
li_planta	=	Integer(istr_mant.argumento[1])

istr_mant.argumento[2]	=	This.Text

ll_numero					=	Long(This.Text)

SELECT altu_fecmov, altu_observ
INTO   :ld_fecha, :ls_obser
FROM dbo.alpalletencab
WHERE clie_codigo = :li_cliente
AND   plde_codigo = :li_planta
AND   altu_numero = :ll_numero;

IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de tabla Alpalletencab")
	em_numero.SetFocus()
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención", "Número de Proceso No Existe !!!!.")
	em_numero.text	=	''
	em_numero.SetFocus()
END IF

istr_mant.argumento[6]	=	String(ld_fecha)
istr_mant.argumento[7]	=	ls_obser




end event

type dw_2 from datawindow within w_info_cambioaltura
integer x = 855
integer y = 492
integer width = 1230
integer height = 96
integer taborder = 10
boolean bringtotop = true
string dataobject = "dddw_clientesprod"
boolean border = false
boolean livescroll = true
end type

event itemchanged;String	ls_Columna[]

IF ExisteCliente(Integer(data), ls_Columna[]) THEN
	istr_mant.argumento[3]	=	data
	istr_mant.argumento[8]	=	ls_Columna[1]
	
	idwc_planta.Retrieve(1)
ELSE
	This.SetItem(1, "clie_codigo", gi_CodExport)
	
	RETURN 1
END IF
end event

event itemerror;RETURN 1
end event

type st_6 from statictext within w_info_cambioaltura
integer x = 347
integer y = 500
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

type dw_1 from datawindow within w_info_cambioaltura
integer x = 855
integer y = 612
integer width = 969
integer height = 92
integer taborder = 20
boolean bringtotop = true
string title = "none"
string dataobject = "dddw_plantadesp_tipo"
boolean border = false
boolean livescroll = true
end type

event itemchanged;String	ls_Columna[]
Integer	li_Cliente

li_Cliente	=	dw_2.Object.clie_codigo[1]

IF ExistePlanta(li_Cliente, Integer(data), ls_Columna[]) THEN
	istr_mant.argumento[1]	=	data
	istr_mant.Argumento[5]	=	ls_Columna[1]
ELSE
	This.SetItem(1, "plde_codigo", gi_CodPlanta)
	
	RETURN 1
END IF
end event

event itemerror;RETURN 1
end event

type cb_buscarepa from commandbutton within w_info_cambioaltura
integer x = 1271
integer y = 848
integer width = 91
integer height = 84
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "..."
end type

event clicked;istr_busq.argum[1]	=	String(dw_2.Object.clie_codigo[1])
istr_busq.argum[2]	=	String(dw_1.Object.plde_codigo[1])
istr_busq.argum[3]	=	'6'

OpenWithParm(w_busc_cambioaltura, istr_busq)

istr_busq	       = Message.PowerObjectParm

IF istr_busq.argum[5] <> "" THEN
	em_numero.Text				= istr_busq.argum[5]
	istr_mant.argumento[2]	= istr_busq.argum[5]
	istr_mant.argumento[6]	= istr_busq.argum[6]
	istr_mant.argumento[7]	= istr_busq.argum[7]	
ELSE
	em_numero.SetFocus()
END IF
end event

type st_7 from statictext within w_info_cambioaltura
integer x = 251
integer y = 992
integer width = 1902
integer height = 212
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

type rb_1 from radiobutton within w_info_cambioaltura
integer x = 489
integer y = 1056
integer width = 535
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
string text = "Listado Pallet"
boolean checked = true
end type

type rb_2 from radiobutton within w_info_cambioaltura
integer x = 1157
integer y = 1056
integer width = 850
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
string text = "Informe para Cuadratura"
end type

