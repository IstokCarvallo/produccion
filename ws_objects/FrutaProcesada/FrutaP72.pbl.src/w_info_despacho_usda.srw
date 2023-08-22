$PBExportHeader$w_info_despacho_usda.srw
$PBExportComments$Informe Consolidado General de Embarques.
forward
global type w_info_despacho_usda from w_para_informes
end type
type dw_cliente from datawindow within w_info_despacho_usda
end type
type st_2 from statictext within w_info_despacho_usda
end type
type em_fdesde from editmask within w_info_despacho_usda
end type
type st_8 from statictext within w_info_despacho_usda
end type
type dw_planta from datawindow within w_info_despacho_usda
end type
type em_hasta from editmask within w_info_despacho_usda
end type
type st_14 from statictext within w_info_despacho_usda
end type
type gb_4 from groupbox within w_info_despacho_usda
end type
type st_12 from statictext within w_info_despacho_usda
end type
type cbx_cliente from checkbox within w_info_despacho_usda
end type
type cbx_planta from checkbox within w_info_despacho_usda
end type
type st_1 from statictext within w_info_despacho_usda
end type
end forward

global type w_info_despacho_usda from w_para_informes
integer width = 2944
integer height = 1372
string title = "INFORME DESPACHO USDA"
boolean maxbox = false
boolean resizable = false
dw_cliente dw_cliente
st_2 st_2
em_fdesde em_fdesde
st_8 st_8
dw_planta dw_planta
em_hasta em_hasta
st_14 st_14
gb_4 gb_4
st_12 st_12
cbx_cliente cbx_cliente
cbx_planta cbx_planta
st_1 st_1
end type
global w_info_despacho_usda w_info_despacho_usda

type variables
str_mant istr_mant
DataWindowChild 		idwc_cliente, idwc_planta

Integer	ii_Cliente, ii_Planta
String	is_patente
Boolean	ib_conectado
Transaction	sqlconec



end variables

on w_info_despacho_usda.create
int iCurrent
call super::create
this.dw_cliente=create dw_cliente
this.st_2=create st_2
this.em_fdesde=create em_fdesde
this.st_8=create st_8
this.dw_planta=create dw_planta
this.em_hasta=create em_hasta
this.st_14=create st_14
this.gb_4=create gb_4
this.st_12=create st_12
this.cbx_cliente=create cbx_cliente
this.cbx_planta=create cbx_planta
this.st_1=create st_1
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_cliente
this.Control[iCurrent+2]=this.st_2
this.Control[iCurrent+3]=this.em_fdesde
this.Control[iCurrent+4]=this.st_8
this.Control[iCurrent+5]=this.dw_planta
this.Control[iCurrent+6]=this.em_hasta
this.Control[iCurrent+7]=this.st_14
this.Control[iCurrent+8]=this.gb_4
this.Control[iCurrent+9]=this.st_12
this.Control[iCurrent+10]=this.cbx_cliente
this.Control[iCurrent+11]=this.cbx_planta
this.Control[iCurrent+12]=this.st_1
end on

on w_info_despacho_usda.destroy
call super::destroy
destroy(this.dw_cliente)
destroy(this.st_2)
destroy(this.em_fdesde)
destroy(this.st_8)
destroy(this.dw_planta)
destroy(this.em_hasta)
destroy(this.st_14)
destroy(this.gb_4)
destroy(this.st_12)
destroy(this.cbx_cliente)
destroy(this.cbx_planta)
destroy(this.st_1)
end on

event open;call super::open;x				= 0
y				= 0


dw_cliente.GetChild("clie_codigo", idwc_cliente)
idwc_cliente.SetTransObject(SQLCA)
idwc_cliente.Retrieve()
dw_cliente.InsertRow(0)
dw_cliente.Object.clie_codigo[1]	=	gi_CodExport

dw_planta.GetChild("plde_codigo", idwc_planta)
idwc_planta.SetTransObject(SQLCA)
idwc_planta.Retrieve(1)
dw_planta.InsertRow(0)
dw_planta.Object.plde_codigo[1] = gi_CodPlanta

ii_Cliente						=	gi_CodExport
ii_Planta						=	gi_CodPlanta

em_fdesde.text					=	String(RelativeDate(Today() , -365))
em_hasta.text					=	String(Today())	










end event

type pb_excel from w_para_informes`pb_excel within w_info_despacho_usda
end type

type st_computador from w_para_informes`st_computador within w_info_despacho_usda
end type

type st_usuario from w_para_informes`st_usuario within w_info_despacho_usda
end type

type st_temporada from w_para_informes`st_temporada within w_info_despacho_usda
end type

type p_logo from w_para_informes`p_logo within w_info_despacho_usda
end type

type st_titulo from w_para_informes`st_titulo within w_info_despacho_usda
integer width = 2171
integer height = 104
string text = "Informe Despacho USDA"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_despacho_usda
integer x = 2555
integer y = 628
integer taborder = 160
end type

event pb_acepta::clicked;Long	  	ll_Fila
String	t_fecha, ls_cajas

SetPointer(HourGlass!)

istr_info.titulo	= 'INFORME DESPACHO USDA'

OpenWithParm(vinf, istr_info)

vinf.dw_1.DataObject = "dw_info_despachousda"

vinf.dw_1.SetTransObject(sqlca)

ll_Fila	=	vinf.dw_1.Retrieve(ii_Cliente,ii_Planta,Date(em_fdesde.text),Date(em_hasta.text))

IF ll_Fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
				"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ELSEIF ll_Fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", &
	StopSign!, Ok!)
ELSE
	F_Membrete(vinf.dw_1)

	vinf.dw_1.Modify("fechadesde.text = '" + em_fdesde.text + "'")
	vinf.dw_1.Modify("fechahasta.text = '" + em_hasta.text + "'")
		
	IF gs_Ambiente <> 'Windows' THEN
		F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
	END IF
END IF
end event

type pb_salir from w_para_informes`pb_salir within w_info_despacho_usda
integer x = 2555
integer y = 916
integer taborder = 170
end type

type dw_cliente from datawindow within w_info_despacho_usda
integer x = 896
integer y = 556
integer width = 1157
integer height = 92
integer taborder = 10
boolean bringtotop = true
string title = "none"
string dataobject = "dddw_clientesprod"
boolean border = false
boolean livescroll = true
end type

event itemchanged;String	ls_Columna[]
Integer li_nula

SetNull(li_nula)

IF ExisteCliente(Integer(data), ls_Columna[]) THEN
	
	ii_cliente	=	integer(data)
	dw_planta.GetChild("plde_codigo", idwc_planta)
	idwc_planta.SetTransObject(SQLCA)
	idwc_planta.Retrieve(1)
ELSE
	This.SetItem(1, "clie_codigo", li_nula)
	RETURN 1
END IF
end event

event itemerror;RETURN 1
end event

type st_2 from statictext within w_info_despacho_usda
integer x = 343
integer y = 556
integer width = 270
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

type em_fdesde from editmask within w_info_despacho_usda
integer x = 896
integer y = 896
integer width = 402
integer height = 92
integer taborder = 130
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

type st_8 from statictext within w_info_despacho_usda
integer x = 343
integer y = 660
integer width = 270
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
string text = "Planta"
boolean focusrectangle = false
end type

type dw_planta from datawindow within w_info_despacho_usda
integer x = 896
integer y = 660
integer width = 1157
integer height = 92
integer taborder = 20
boolean bringtotop = true
string title = "none"
string dataobject = "dddw_plantadesp_tipo"
boolean border = false
boolean livescroll = true
end type

event itemchanged;String	ls_Columna[]
Integer	li_Nula

SetNull(li_Nula)

IF ExistePlanta(ii_Cliente,Integer(data), ls_Columna[]) THEN
	ii_Planta		=	Integer(data)

ELSE
	This.SetItem(1, "plde_codigo", li_Nula)
	RETURN 1
END IF
end event

event itemerror;RETURN 1
end event

type em_hasta from editmask within w_info_despacho_usda
integer x = 1710
integer y = 896
integer width = 402
integer height = 92
integer taborder = 140
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

type st_14 from statictext within w_info_despacho_usda
integer x = 1467
integer y = 920
integer width = 206
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
string text = "Hasta"
boolean focusrectangle = false
end type

type gb_4 from groupbox within w_info_despacho_usda
integer x = 270
integer y = 832
integer width = 2126
integer height = 172
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Fecha"
end type

type st_12 from statictext within w_info_despacho_usda
integer x = 576
integer y = 920
integer width = 256
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
string text = "Desde"
boolean focusrectangle = false
end type

type cbx_cliente from checkbox within w_info_despacho_usda
boolean visible = false
integer x = 2304
integer y = 1128
integer width = 311
integer height = 80
integer taborder = 50
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 553648127
boolean enabled = false
string text = "Todos"
end type

event clicked;Integer li_null
SetNull(li_null)
IF This.Checked THEN
	ii_Cliente = -1
	dw_cliente.SetItem(1,"clie_codigo",li_null)
	dw_cliente.Object.clie_codigo.Protect	=	1
	dw_cliente.Object.clie_codigo.BackGround.Color	=	RGB(166,180,210)
ELSE
	dw_cliente.Object.clie_codigo.Protect	=	0
	dw_cliente.Object.clie_codigo.BackGround.Color	=	RGB(255,255,255)
	dw_cliente.SetFocus()
END IF
end event

type cbx_planta from checkbox within w_info_despacho_usda
boolean visible = false
integer x = 2304
integer y = 1232
integer width = 311
integer height = 80
integer taborder = 50
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 553648127
boolean enabled = false
string text = "Todos"
end type

event clicked;Integer li_null
SetNull(li_null)
IF This.Checked THEN
	ii_Planta = -1
	dw_planta.SetItem(1,"plde_codigo",li_null)
	dw_planta.Object.plde_codigo.Protect	=	1
	dw_planta.Object.plde_codigo.BackGround.Color	=	RGB(166,180,210)
ELSE
	dw_planta.Object.plde_codigo.Protect	=	0
	dw_planta.Object.plde_codigo.BackGround.Color	=	RGB(255,255,255)
	dw_planta.SetFocus()
END IF
end event

type st_1 from statictext within w_info_despacho_usda
integer x = 247
integer y = 440
integer width = 2171
integer height = 640
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 16711680
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

