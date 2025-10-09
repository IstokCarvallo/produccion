$PBExportHeader$w_info_cuadratura_despachos.srw
forward
global type w_info_cuadratura_despachos from w_para_informes
end type
type st_6 from statictext within w_info_cuadratura_despachos
end type
type st_especie from statictext within w_info_cuadratura_despachos
end type
type st_variedad from statictext within w_info_cuadratura_despachos
end type
type st_3 from statictext within w_info_cuadratura_despachos
end type
type uo_selcliente from uo_seleccion_clientesprod within w_info_cuadratura_despachos
end type
type uo_selplanta from uo_seleccion_plantas within w_info_cuadratura_despachos
end type
type em_desde from editmask within w_info_cuadratura_despachos
end type
type em_hasta from editmask within w_info_cuadratura_despachos
end type
type dw_1 from uo_dw within w_info_cuadratura_despachos
end type
type st_1 from statictext within w_info_cuadratura_despachos
end type
type em_edesde from editmask within w_info_cuadratura_despachos
end type
type st_2 from statictext within w_info_cuadratura_despachos
end type
type em_ehasta from editmask within w_info_cuadratura_despachos
end type
type gb_4 from groupbox within w_info_cuadratura_despachos
end type
type st_4 from statictext within w_info_cuadratura_despachos
end type
type gb_5 from groupbox within w_info_cuadratura_despachos
end type
type st_nro2 from statictext within w_info_cuadratura_despachos
end type
type st_5 from statictext within w_info_cuadratura_despachos
end type
type uo_selespecie from uo_seleccion_especie within w_info_cuadratura_despachos
end type
end forward

global type w_info_cuadratura_despachos from w_para_informes
integer x = 14
integer y = 32
integer width = 2619
integer height = 1800
string title = "Excel Cuadratura despachos"
boolean minbox = false
boolean maxbox = false
boolean resizable = false
windowtype windowtype = response!
string icon = "F:\Desarrollo\Producción\FrutaProcesada\Producc.ico"
st_6 st_6
st_especie st_especie
st_variedad st_variedad
st_3 st_3
uo_selcliente uo_selcliente
uo_selplanta uo_selplanta
em_desde em_desde
em_hasta em_hasta
dw_1 dw_1
st_1 st_1
em_edesde em_edesde
st_2 st_2
em_ehasta em_ehasta
gb_4 gb_4
st_4 st_4
gb_5 gb_5
st_nro2 st_nro2
st_5 st_5
uo_selespecie uo_selespecie
end type
global w_info_cuadratura_despachos w_info_cuadratura_despachos

type variables
str_busqueda istr_busq
str_mant istr_mant

DataWindowChild	idwc_cliente,idwc_planta,idwc_etiqueta,idwc_tipopallemba,idwc_Destino,&
						idwc_stat, idwc_copal, idwc_tipofrio, dwc_descripcion,idwc_stat2,idwc_stat3,idwc_stat4,idwc_stat5,idwc_stat6

uo_seleccion_especie					iuo_selespecie
uo_seleccion_variedad				iuo_selvariedad
uo_seleccion_varios_productores	iuo_selproductor
uo_calibre								iuo_calibre

 
Integer	ii_variable, ii_tipoi, ii_calificacion, il_secuencia, ii_calificacion2	
Long		ll_norden
String	is_frio, is_descripcion
end variables

on w_info_cuadratura_despachos.create
int iCurrent
call super::create
this.st_6=create st_6
this.st_especie=create st_especie
this.st_variedad=create st_variedad
this.st_3=create st_3
this.uo_selcliente=create uo_selcliente
this.uo_selplanta=create uo_selplanta
this.em_desde=create em_desde
this.em_hasta=create em_hasta
this.dw_1=create dw_1
this.st_1=create st_1
this.em_edesde=create em_edesde
this.st_2=create st_2
this.em_ehasta=create em_ehasta
this.gb_4=create gb_4
this.st_4=create st_4
this.gb_5=create gb_5
this.st_nro2=create st_nro2
this.st_5=create st_5
this.uo_selespecie=create uo_selespecie
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_6
this.Control[iCurrent+2]=this.st_especie
this.Control[iCurrent+3]=this.st_variedad
this.Control[iCurrent+4]=this.st_3
this.Control[iCurrent+5]=this.uo_selcliente
this.Control[iCurrent+6]=this.uo_selplanta
this.Control[iCurrent+7]=this.em_desde
this.Control[iCurrent+8]=this.em_hasta
this.Control[iCurrent+9]=this.dw_1
this.Control[iCurrent+10]=this.st_1
this.Control[iCurrent+11]=this.em_edesde
this.Control[iCurrent+12]=this.st_2
this.Control[iCurrent+13]=this.em_ehasta
this.Control[iCurrent+14]=this.gb_4
this.Control[iCurrent+15]=this.st_4
this.Control[iCurrent+16]=this.gb_5
this.Control[iCurrent+17]=this.st_nro2
this.Control[iCurrent+18]=this.st_5
this.Control[iCurrent+19]=this.uo_selespecie
end on

on w_info_cuadratura_despachos.destroy
call super::destroy
destroy(this.st_6)
destroy(this.st_especie)
destroy(this.st_variedad)
destroy(this.st_3)
destroy(this.uo_selcliente)
destroy(this.uo_selplanta)
destroy(this.em_desde)
destroy(this.em_hasta)
destroy(this.dw_1)
destroy(this.st_1)
destroy(this.em_edesde)
destroy(this.st_2)
destroy(this.em_ehasta)
destroy(this.gb_4)
destroy(this.st_4)
destroy(this.gb_5)
destroy(this.st_nro2)
destroy(this.st_5)
destroy(this.uo_selespecie)
end on

event open;call super::open;Date		ld_fecha, ld_actual
Boolean	lb_Cerrar

If IsNull(uo_SelCliente.Codigo) Then lb_Cerrar = True
If IsNull(uo_SelPlanta.Codigo) Then lb_Cerrar = True
If IsNull(uo_SelEspecie.Codigo) Then lb_Cerrar = True

If lb_Cerrar Then
	Close(This)
Else	
	uo_SelCliente.Seleccion(True, False)
	uo_SelPlanta.Seleccion(True, False)
	uo_SelEspecie.Seleccion(True, False)
	
	
	ld_actual	=	Today()
	ld_fecha	=	RelativeDate(ld_actual, -365)
	
	em_desde.Text	=	String(ld_fecha)
	em_hasta.Text	=	String(ld_actual)
	em_Edesde.Text	=	String(ld_fecha)
	em_Ehasta.Text	=	String(ld_actual)
	
//	uo_SelCliente.Codigo	= gi_CodExport
//	uo_SelCliente.dw_Seleccion.Object.Codigo[1]	=	gi_CodExport
	
	dw_1.SetTransObject(Sqlca)
End If
end event

type pb_excel from w_para_informes`pb_excel within w_info_cuadratura_despachos
boolean visible = true
integer x = 2254
integer y = 256
boolean enabled = true
boolean default = true
end type

event pb_excel::clicked;call super::clicked;SetPointer(Arrow!)

Long		ll_Fila
Integer	li_Resultado
String		ls_Archivo, ls_Ruta

//RegistryGet( "HKEY_CURRENT_USER\Software\Microsoft\Windows\CurrentVersion\Explorer\Shell Folders", "Personal", RegString!, ls_Ruta)
//ls_Archivo	= 'LiquidacionDespachos_' + Mid(em_Desde.Text, 1, 2) + Mid(em_Desde.Text, 4, 2) + Mid(em_Desde.Text, 7, 4) + '.xlsx'

ll_Fila	=	dw_1.Retrieve(uo_SelCliente.Codigo, uo_SelPlanta.Codigo, uo_SelEspecie.Codigo, &
			Date(em_Desde.Text), Date(em_Hasta.Text), Date(em_EDesde.Text), Date(em_EHasta.Text))

If ll_Fila = -1 Then
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
			   	"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ElseIf ll_Fila = 0 Then
	MessageBox( "No Existe información", "No existe información para este informe.", StopSign!, Ok!)
Else
	If dw_1.SaveAs() = -1 Then
		Messagebox('Error', 'Archivo no se pudo generar.', StopSign!, Ok!)
		Return
	Else
		Messagebox('Atencion', 'Archivo genrado en ruta:', Information!, Ok!)
	End If
End If

SetPointer(Arrow!)
end event

type st_computador from w_para_informes`st_computador within w_info_cuadratura_despachos
end type

type st_usuario from w_para_informes`st_usuario within w_info_cuadratura_despachos
end type

type st_temporada from w_para_informes`st_temporada within w_info_cuadratura_despachos
end type

type p_logo from w_para_informes`p_logo within w_info_cuadratura_despachos
string picturename = "\Desarrollo 17\Imagenes\Logos\RBlanco.jpg"
end type

type st_titulo from w_para_informes`st_titulo within w_info_cuadratura_despachos
integer width = 1728
integer height = 92
string text = "Cuadratura despachos"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_cuadratura_despachos
string tag = "Imprimir Reporte"
boolean visible = false
integer x = 2263
integer y = 520
integer taborder = 520
integer weight = 400
fontcharset fontcharset = ansi!
boolean enabled = false
boolean default = false
string picturename = "\Desarrollo 17\Imagenes\Botones\Aceptar.png"
string disabledname = "\Desarrollo 17\Imagenes\Botones\Aceptar-bn.png"
end type

type pb_salir from w_para_informes`pb_salir within w_info_cuadratura_despachos
string tag = "Salir [Cerrar Ventana Activa]"
integer x = 2263
integer y = 804
integer taborder = 530
string powertiptext = "Salir [Cerrar Ventana Activa]"
end type

type st_6 from statictext within w_info_cuadratura_despachos
integer x = 338
integer y = 520
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

type st_especie from statictext within w_info_cuadratura_despachos
integer x = 320
integer y = 1152
integer width = 238
integer height = 60
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

type st_variedad from statictext within w_info_cuadratura_despachos
integer x = 1198
integer y = 1152
integer width = 279
integer height = 60
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

type st_3 from statictext within w_info_cuadratura_despachos
integer x = 338
integer y = 704
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
string text = "Planta"
boolean focusrectangle = false
end type

type uo_selcliente from uo_seleccion_clientesprod within w_info_cuadratura_despachos
event destroy ( )
integer x = 645
integer y = 428
integer taborder = 530
boolean bringtotop = true
end type

on uo_selcliente.destroy
call uo_seleccion_clientesprod::destroy
end on

type uo_selplanta from uo_seleccion_plantas within w_info_cuadratura_despachos
event destroy ( )
integer x = 645
integer y = 612
integer taborder = 530
boolean bringtotop = true
end type

on uo_selplanta.destroy
call uo_seleccion_plantas::destroy
end on

type em_desde from editmask within w_info_cuadratura_despachos
integer x = 599
integer y = 1136
integer width = 507
integer height = 92
integer taborder = 540
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
boolean dropdowncalendar = true
end type

type em_hasta from editmask within w_info_cuadratura_despachos
integer x = 1394
integer y = 1136
integer width = 507
integer height = 92
integer taborder = 550
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
boolean dropdowncalendar = true
end type

type dw_1 from uo_dw within w_info_cuadratura_despachos
boolean visible = false
integer x = 1330
integer width = 270
integer height = 192
integer taborder = 11
boolean bringtotop = true
string dataobject = "dw_info_cuadraturadespachos"
boolean vscrollbar = false
end type

type st_1 from statictext within w_info_cuadratura_despachos
integer x = 306
integer y = 1388
integer width = 238
integer height = 60
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

type em_edesde from editmask within w_info_cuadratura_despachos
integer x = 599
integer y = 1372
integer width = 507
integer height = 92
integer taborder = 550
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
boolean dropdowncalendar = true
end type

type st_2 from statictext within w_info_cuadratura_despachos
integer x = 1184
integer y = 1388
integer width = 279
integer height = 60
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

type em_ehasta from editmask within w_info_cuadratura_despachos
integer x = 1394
integer y = 1372
integer width = 507
integer height = 92
integer taborder = 550
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
boolean dropdowncalendar = true
end type

type gb_4 from groupbox within w_info_cuadratura_despachos
integer x = 293
integer y = 1288
integer width = 1637
integer height = 228
integer taborder = 540
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = " Fecha Embalaje "
end type

type st_4 from statictext within w_info_cuadratura_despachos
integer x = 247
integer y = 404
integer width = 1728
integer height = 632
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long backcolor = 16711680
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type gb_5 from groupbox within w_info_cuadratura_despachos
integer x = 293
integer y = 1060
integer width = 1637
integer height = 228
integer taborder = 550
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = " Fecha despachos "
end type

type st_nro2 from statictext within w_info_cuadratura_despachos
integer x = 242
integer y = 1040
integer width = 1728
integer height = 516
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long backcolor = 16711680
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_5 from statictext within w_info_cuadratura_despachos
integer x = 338
integer y = 924
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
string text = "Especie"
boolean focusrectangle = false
end type

type uo_selespecie from uo_seleccion_especie within w_info_cuadratura_despachos
integer x = 645
integer y = 832
integer taborder = 540
boolean bringtotop = true
end type

on uo_selespecie.destroy
call uo_seleccion_especie::destroy
end on

