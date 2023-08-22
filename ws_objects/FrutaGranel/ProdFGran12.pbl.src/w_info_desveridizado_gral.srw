$PBExportHeader$w_info_desveridizado_gral.srw
forward
global type w_info_desveridizado_gral from w_para_informes
end type
type gb_3 from groupbox within w_info_desveridizado_gral
end type
type st_6 from statictext within w_info_desveridizado_gral
end type
type st_8 from statictext within w_info_desveridizado_gral
end type
type st_3 from statictext within w_info_desveridizado_gral
end type
type st_variedad from statictext within w_info_desveridizado_gral
end type
type st_5 from statictext within w_info_desveridizado_gral
end type
type st_4 from statictext within w_info_desveridizado_gral
end type
type uo_selespecie from uo_seleccion_especie within w_info_desveridizado_gral
end type
type uo_selproductor from uo_seleccion_productor within w_info_desveridizado_gral
end type
type uo_selvariedad from uo_seleccion_variedad within w_info_desveridizado_gral
end type
type st_2 from statictext within w_info_desveridizado_gral
end type
type em_desde from editmask within w_info_desveridizado_gral
end type
type st_7 from statictext within w_info_desveridizado_gral
end type
type em_hasta from editmask within w_info_desveridizado_gral
end type
type cbx_todosfecha from checkbox within w_info_desveridizado_gral
end type
type st_10 from statictext within w_info_desveridizado_gral
end type
type uo_selcliente from uo_seleccion_clientesprod within w_info_desveridizado_gral
end type
type dw_1 from datawindow within w_info_desveridizado_gral
end type
type ddlb_status from dropdownlistbox within w_info_desveridizado_gral
end type
type st_1 from statictext within w_info_desveridizado_gral
end type
type cbx_status from checkbox within w_info_desveridizado_gral
end type
type uo_selplanta from uo_seleccion_plantas within w_info_desveridizado_gral
end type
type st_9 from statictext within w_info_desveridizado_gral
end type
type st_11 from statictext within w_info_desveridizado_gral
end type
type uo_selcolor from uo_seleccion_color within w_info_desveridizado_gral
end type
end forward

global type w_info_desveridizado_gral from w_para_informes
integer width = 3447
integer height = 1640
boolean minbox = false
boolean maxbox = false
gb_3 gb_3
st_6 st_6
st_8 st_8
st_3 st_3
st_variedad st_variedad
st_5 st_5
st_4 st_4
uo_selespecie uo_selespecie
uo_selproductor uo_selproductor
uo_selvariedad uo_selvariedad
st_2 st_2
em_desde em_desde
st_7 st_7
em_hasta em_hasta
cbx_todosfecha cbx_todosfecha
st_10 st_10
uo_selcliente uo_selcliente
dw_1 dw_1
ddlb_status ddlb_status
st_1 st_1
cbx_status cbx_status
uo_selplanta uo_selplanta
st_9 st_9
st_11 st_11
uo_selcolor uo_selcolor
end type
global w_info_desveridizado_gral w_info_desveridizado_gral

type variables
Integer	ii_status = -1
end variables

forward prototypes
public function boolean noexisteetiqueta (integer li_etiqueta)
end prototypes

public function boolean noexisteetiqueta (integer li_etiqueta);String	ls_nombre


SELECT	etiq_nombre
INTO    :ls_nombre
FROM	dba.etiquetas
WHERE	etiq_codigo =  :li_etiqueta;
	
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de tabla Etiquetas")
	RETURN True
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención", "Código de Etiqueta no ha sido Definido.~r~r" + &
					"Ingrese o seleccione otro Código.")
	RETURN True
ELSE
	RETURN False 
END IF
end function

on w_info_desveridizado_gral.create
int iCurrent
call super::create
this.gb_3=create gb_3
this.st_6=create st_6
this.st_8=create st_8
this.st_3=create st_3
this.st_variedad=create st_variedad
this.st_5=create st_5
this.st_4=create st_4
this.uo_selespecie=create uo_selespecie
this.uo_selproductor=create uo_selproductor
this.uo_selvariedad=create uo_selvariedad
this.st_2=create st_2
this.em_desde=create em_desde
this.st_7=create st_7
this.em_hasta=create em_hasta
this.cbx_todosfecha=create cbx_todosfecha
this.st_10=create st_10
this.uo_selcliente=create uo_selcliente
this.dw_1=create dw_1
this.ddlb_status=create ddlb_status
this.st_1=create st_1
this.cbx_status=create cbx_status
this.uo_selplanta=create uo_selplanta
this.st_9=create st_9
this.st_11=create st_11
this.uo_selcolor=create uo_selcolor
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.gb_3
this.Control[iCurrent+2]=this.st_6
this.Control[iCurrent+3]=this.st_8
this.Control[iCurrent+4]=this.st_3
this.Control[iCurrent+5]=this.st_variedad
this.Control[iCurrent+6]=this.st_5
this.Control[iCurrent+7]=this.st_4
this.Control[iCurrent+8]=this.uo_selespecie
this.Control[iCurrent+9]=this.uo_selproductor
this.Control[iCurrent+10]=this.uo_selvariedad
this.Control[iCurrent+11]=this.st_2
this.Control[iCurrent+12]=this.em_desde
this.Control[iCurrent+13]=this.st_7
this.Control[iCurrent+14]=this.em_hasta
this.Control[iCurrent+15]=this.cbx_todosfecha
this.Control[iCurrent+16]=this.st_10
this.Control[iCurrent+17]=this.uo_selcliente
this.Control[iCurrent+18]=this.dw_1
this.Control[iCurrent+19]=this.ddlb_status
this.Control[iCurrent+20]=this.st_1
this.Control[iCurrent+21]=this.cbx_status
this.Control[iCurrent+22]=this.uo_selplanta
this.Control[iCurrent+23]=this.st_9
this.Control[iCurrent+24]=this.st_11
this.Control[iCurrent+25]=this.uo_selcolor
end on

on w_info_desveridizado_gral.destroy
call super::destroy
destroy(this.gb_3)
destroy(this.st_6)
destroy(this.st_8)
destroy(this.st_3)
destroy(this.st_variedad)
destroy(this.st_5)
destroy(this.st_4)
destroy(this.uo_selespecie)
destroy(this.uo_selproductor)
destroy(this.uo_selvariedad)
destroy(this.st_2)
destroy(this.em_desde)
destroy(this.st_7)
destroy(this.em_hasta)
destroy(this.cbx_todosfecha)
destroy(this.st_10)
destroy(this.uo_selcliente)
destroy(this.dw_1)
destroy(this.ddlb_status)
destroy(this.st_1)
destroy(this.cbx_status)
destroy(this.uo_selplanta)
destroy(this.st_9)
destroy(this.st_11)
destroy(this.uo_selcolor)
end on

event open;call super::open;Boolean lb_cerrar

IF IsNull(uo_SelCliente.Codigo) 	THEN lb_Cerrar	=	True
IF IsNull(uo_SelPlanta.Codigo) 	THEN lb_Cerrar	=	True
IF IsNull(uo_SelProductor.Codigo)THEN lb_Cerrar	=	True
IF IsNull(uo_SelEspecie.Codigo) 	THEN lb_Cerrar	=	True
IF IsNull(uo_SelVariedad.Codigo) THEN lb_Cerrar	=	True
IF IsNull(uo_SelColor.Codigo) 	THEN lb_Cerrar	=	True

IF lb_Cerrar THEN
	Close(This)
Else
	uo_SelCliente.Seleccion(True, False)
	uo_SelPlanta.Seleccion(True, False)
	uo_SelEspecie.Seleccion(True, False)
	uo_SelVariedad.Seleccion(True, False)
	uo_SelProductor.Seleccion(True, False)
	uo_SelColor.Seleccion(True, False)
	
	uo_SelProductor.Filtra(-1)
	uo_SelColor.Filtra(-1)
	
	cbx_todosfecha.TriggerEvent("clicked")
	
	em_desde.Text 	= 	String(RelativeDate(Today(), -365))
	em_hasta.Text 		= 	String(Today())
End If
end event

type pb_excel from w_para_informes`pb_excel within w_info_desveridizado_gral
boolean visible = true
integer x = 3081
integer y = 416
integer taborder = 0
boolean enabled = true
boolean default = true
end type

event pb_excel::clicked;call super::clicked;SetPointer(HourGlass!)

Long		ll_fila, ll_cierre
String 	ls_path, ls_file

dw_1.SetTransObject(sqlca)

ll_fila = dw_1.Retrieve(uo_SelCliente.Codigo, uo_SelPlanta.Codigo, uo_SelEspecie.Codigo, uo_SelVariedad.Codigo, uo_SelProductor.Codigo, &
						ii_status, uo_SelColor.Codigo, DateTime(em_Desde.Text), DateTime(em_Hasta.Text))

If ll_fila = -1 Then
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ElseIf ll_fila = 0 Then
	MessageBox( "No Existe información", "No existe información para este informe.", StopSign!, Ok!)
Else

	If GetFileSaveName( "Seleccione archivo",  ls_path, ls_file, "Excel", ".XLS Files (*.xls),*.xls" , "C:\") = -1 Then
		MessageBox('Error', 'No se encontro archivo solicitdo.' , StopSign!, OK! )
		Return -1
	End If

	If dw_1.SaveAs(ls_File, Excel8!, True) = -1 Then
		MessageBox('Error', 'No se pùdo generar archivo ('+ ls_file +') con informción solicitda.' , StopSign!, OK! )
		Return -1
	Else
		MessageBox('Atencion', 'Archivo ('+ ls_file +') generado satisfactoriamente.' , Information!, OK! )
	End If
End If

SetPointer(Arrow!)

end event

type st_computador from w_para_informes`st_computador within w_info_desveridizado_gral
end type

type st_usuario from w_para_informes`st_usuario within w_info_desveridizado_gral
end type

type st_temporada from w_para_informes`st_temporada within w_info_desveridizado_gral
end type

type p_logo from w_para_informes`p_logo within w_info_desveridizado_gral
integer width = 411
string picturename = "\Desarrollo 17\Imagenes\Logos\RBlanco.jpg"
end type

type st_titulo from w_para_informes`st_titulo within w_info_desveridizado_gral
integer x = 242
integer width = 2674
string text = "Informe de Lotes Desverdizados"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_desveridizado_gral
boolean visible = false
integer x = 3022
integer y = 712
integer taborder = 100
fontcharset fontcharset = ansi!
boolean enabled = false
boolean default = false
end type

event pb_acepta::clicked;Integer		li_consfecha
String			ls_Archivo,	ls_ruta
Long			ll_nroorden, 	fila
Date			ld_desde, ld_hasta

SetPointer(HourGlass!)

istr_info.titulo	= "INFORME DE LOTES DESVERDIZADOS"
istr_info.copias	= 1

li_ConsFecha = 1
ld_desde			=	Date(em_desde.Text)
ld_hasta			=	Date(em_hasta.Text)

OpenWithParm(vinf, istr_info)
vinf.dw_1.DataObject = ""
vinf.dw_1.SetTransObject(sqlca)

fila = vinf.dw_1.Retrieve(uo_SelCliente.Codigo, uo_SelProductor.Codigo, uo_SelEspecie.Codigo, uo_SelVariedad.Codigo, ld_desde,ld_hasta)

IF fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ELSEIF fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", StopSign!, Ok!)
ELSE
	IF Not gs_Ambiente = 'Windows' THEN F_ImprimeInformePdf(vinf.dw_1, "TituloInforme")
END IF

SetPointer(Arrow!)
end event

type pb_salir from w_para_informes`pb_salir within w_info_desveridizado_gral
integer x = 3013
integer y = 1004
integer taborder = 110
fontcharset fontcharset = ansi!
end type

type gb_3 from groupbox within w_info_desveridizado_gral
integer x = 599
integer y = 1240
integer width = 1957
integer height = 196
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554431
long backcolor = 16711680
string text = "Fecha Recepcion"
end type

type st_6 from statictext within w_info_desveridizado_gral
integer x = 297
integer y = 548
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

type st_8 from statictext within w_info_desveridizado_gral
integer x = 297
integer y = 728
integer width = 329
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

type st_3 from statictext within w_info_desveridizado_gral
integer x = 1669
integer y = 540
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
string text = "Especie"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_variedad from statictext within w_info_desveridizado_gral
integer x = 1659
integer y = 776
integer width = 302
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
string text = "Variedad"
boolean focusrectangle = false
end type

type st_5 from statictext within w_info_desveridizado_gral
integer x = 1582
integer y = 424
integer width = 1335
integer height = 780
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554431
long backcolor = 16711680
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_4 from statictext within w_info_desveridizado_gral
integer x = 242
integer y = 424
integer width = 1335
integer height = 780
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554431
long backcolor = 16711680
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type uo_selespecie from uo_seleccion_especie within w_info_desveridizado_gral
event destroy ( )
integer x = 1966
integer y = 452
integer taborder = 40
boolean bringtotop = true
end type

on uo_selespecie.destroy
call uo_seleccion_especie::destroy
end on

event ue_cambio;call super::ue_cambio;IF IsNull(This.Codigo) THEN RETURN

CHOOSE CASE This.Codigo
	CASE -1, -9
		uo_SelVariedad.Todos(True)
		uo_SelColor.Todos(True)
		
	CASE ELSE
		uo_SelVariedad.Filtra(This.Codigo)
		uo_SelColor.Filtra(This.Codigo)
		
END CHOOSE
end event

type uo_selproductor from uo_seleccion_productor within w_info_desveridizado_gral
event destroy ( )
integer x = 622
integer y = 824
integer taborder = 30
boolean bringtotop = true
end type

on uo_selproductor.destroy
call uo_seleccion_productor::destroy
end on

type uo_selvariedad from uo_seleccion_variedad within w_info_desveridizado_gral
event destroy ( )
integer x = 1979
integer y = 680
integer taborder = 50
boolean bringtotop = true
end type

on uo_selvariedad.destroy
call uo_seleccion_variedad::destroy
end on

type st_2 from statictext within w_info_desveridizado_gral
integer x = 658
integer y = 1324
integer width = 215
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
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type em_desde from editmask within w_info_desveridizado_gral
integer x = 905
integer y = 1308
integer width = 439
integer height = 96
integer taborder = 80
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
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
end type

type st_7 from statictext within w_info_desveridizado_gral
integer x = 1449
integer y = 1324
integer width = 215
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

type em_hasta from editmask within w_info_desveridizado_gral
integer x = 1723
integer y = 1308
integer width = 439
integer height = 96
integer taborder = 90
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
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
end type

type cbx_todosfecha from checkbox within w_info_desveridizado_gral
integer x = 2235
integer y = 1316
integer width = 293
integer height = 72
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Todas"
boolean checked = true
end type

event clicked;IF This.Checked THEN
	em_desde.Text 		= String(RelativeDate(Today(), -365))
	em_hasta.Text 			= String(Today())
	em_desde.enabled	= False
	em_hasta.enabled		= False
ELSE
	em_desde.enabled	= True
	em_hasta.enabled		= True
END IF
end event

type st_10 from statictext within w_info_desveridizado_gral
integer x = 242
integer y = 1204
integer width = 2674
integer height = 300
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 16711680
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type uo_selcliente from uo_seleccion_clientesprod within w_info_desveridizado_gral
integer x = 622
integer y = 456
integer taborder = 10
boolean bringtotop = true
end type

on uo_selcliente.destroy
call uo_seleccion_clientesprod::destroy
end on

type dw_1 from datawindow within w_info_desveridizado_gral
boolean visible = false
integer x = 3145
integer y = 232
integer width = 151
integer height = 132
boolean bringtotop = true
string title = "none"
string dataobject = "dw_info_desverdizado_gral"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type ddlb_status from dropdownlistbox within w_info_desveridizado_gral
integer x = 622
integer y = 1080
integer width = 896
integer height = 400
integer taborder = 40
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 16777215
boolean enabled = false
boolean sorted = false
string item[] = {"Recepcion","Desverdizado","Disponible"}
borderstyle borderstyle = stylelowered!
end type

event selectionchanged;ii_status = Index - 1
end event

type st_1 from statictext within w_info_desveridizado_gral
integer x = 297
integer y = 1100
integer width = 329
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
string text = "Status"
boolean focusrectangle = false
end type

type cbx_status from checkbox within w_info_desveridizado_gral
integer x = 631
integer y = 1008
integer width = 293
integer height = 72
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Todas"
boolean checked = true
end type

event clicked;If This.Checked Then
	ii_status = -1
	ddlb_status.Enabled = False
Else
	ddlb_status.Enabled = True
End If
end event

type uo_selplanta from uo_seleccion_plantas within w_info_desveridizado_gral
integer x = 622
integer y = 640
integer taborder = 50
boolean bringtotop = true
end type

on uo_selplanta.destroy
call uo_seleccion_plantas::destroy
end on

type st_9 from statictext within w_info_desveridizado_gral
integer x = 297
integer y = 912
integer width = 329
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
string text = "Productor"
boolean focusrectangle = false
end type

type st_11 from statictext within w_info_desveridizado_gral
integer x = 1659
integer y = 1024
integer width = 302
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
string text = "Color"
boolean focusrectangle = false
end type

type uo_selcolor from uo_seleccion_color within w_info_desveridizado_gral
integer x = 1979
integer y = 924
integer taborder = 120
boolean bringtotop = true
end type

on uo_selcolor.destroy
call uo_seleccion_color::destroy
end on

