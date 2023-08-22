$PBExportHeader$w_info_existencia_loslirios.srw
forward
global type w_info_existencia_loslirios from w_para_informes
end type
type st_6 from statictext within w_info_existencia_loslirios
end type
type gb_3 from groupbox within w_info_existencia_loslirios
end type
type st_4 from statictext within w_info_existencia_loslirios
end type
type st_especie from statictext within w_info_existencia_loslirios
end type
type st_nro2 from statictext within w_info_existencia_loslirios
end type
type st_variedad from statictext within w_info_existencia_loslirios
end type
type st_3 from statictext within w_info_existencia_loslirios
end type
type uo_selcliente from uo_seleccion_clientesprod within w_info_existencia_loslirios
end type
type uo_selplanta from uo_seleccion_plantas within w_info_existencia_loslirios
end type
type em_desde from editmask within w_info_existencia_loslirios
end type
type em_hasta from editmask within w_info_existencia_loslirios
end type
type dw_1 from uo_dw within w_info_existencia_loslirios
end type
end forward

global type w_info_existencia_loslirios from w_para_informes
integer x = 14
integer y = 32
integer width = 2551
integer height = 1340
string title = "Archivo Los Lirios"
boolean minbox = false
boolean maxbox = false
boolean resizable = false
windowtype windowtype = response!
string icon = "F:\Desarrollo\Producción\FrutaProcesada\Producc.ico"
st_6 st_6
gb_3 gb_3
st_4 st_4
st_especie st_especie
st_nro2 st_nro2
st_variedad st_variedad
st_3 st_3
uo_selcliente uo_selcliente
uo_selplanta uo_selplanta
em_desde em_desde
em_hasta em_hasta
dw_1 dw_1
end type
global w_info_existencia_loslirios w_info_existencia_loslirios

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

on w_info_existencia_loslirios.create
int iCurrent
call super::create
this.st_6=create st_6
this.gb_3=create gb_3
this.st_4=create st_4
this.st_especie=create st_especie
this.st_nro2=create st_nro2
this.st_variedad=create st_variedad
this.st_3=create st_3
this.uo_selcliente=create uo_selcliente
this.uo_selplanta=create uo_selplanta
this.em_desde=create em_desde
this.em_hasta=create em_hasta
this.dw_1=create dw_1
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_6
this.Control[iCurrent+2]=this.gb_3
this.Control[iCurrent+3]=this.st_4
this.Control[iCurrent+4]=this.st_especie
this.Control[iCurrent+5]=this.st_nro2
this.Control[iCurrent+6]=this.st_variedad
this.Control[iCurrent+7]=this.st_3
this.Control[iCurrent+8]=this.uo_selcliente
this.Control[iCurrent+9]=this.uo_selplanta
this.Control[iCurrent+10]=this.em_desde
this.Control[iCurrent+11]=this.em_hasta
this.Control[iCurrent+12]=this.dw_1
end on

on w_info_existencia_loslirios.destroy
call super::destroy
destroy(this.st_6)
destroy(this.gb_3)
destroy(this.st_4)
destroy(this.st_especie)
destroy(this.st_nro2)
destroy(this.st_variedad)
destroy(this.st_3)
destroy(this.uo_selcliente)
destroy(this.uo_selplanta)
destroy(this.em_desde)
destroy(this.em_hasta)
destroy(this.dw_1)
end on

event open;call super::open;Date		ld_fecha, ld_actual
Boolean	lb_Cerrar

If IsNull(uo_SelCliente.Codigo) Then lb_Cerrar = True
If IsNull(uo_SelPlanta.Codigo) Then lb_Cerrar = True

If lb_Cerrar Then
	Close(This)
Else	
	uo_SelCliente.Seleccion(False, False)
	uo_SelPlanta.Seleccion(True, False)
	
	ld_actual	=	Today()
	ld_fecha	=	RelativeDate(ld_actual, -365)
	
	em_desde.Text	=	String(ld_fecha)
	em_hasta.Text	=	String(ld_actual)
	
	uo_SelCliente.Codigo	= gi_CodExport
//	uo_SelPlanta.Codigo	= gi_CodPlanta
	uo_SelCliente.dw_Seleccion.Object.Codigo[1]	=	gi_CodExport
//	uo_SelPlanta.dw_Seleccion.Object.Codigo[1]	=	gi_CodPlanta
	
	dw_1.SetTransObject(Sqlca)
End If
end event

type st_computador from w_para_informes`st_computador within w_info_existencia_loslirios
end type

type st_usuario from w_para_informes`st_usuario within w_info_existencia_loslirios
end type

type st_temporada from w_para_informes`st_temporada within w_info_existencia_loslirios
end type

type p_logo from w_para_informes`p_logo within w_info_existencia_loslirios
end type

type st_titulo from w_para_informes`st_titulo within w_info_existencia_loslirios
integer width = 1728
integer height = 92
string text = "Archivo Los Lirios"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_existencia_loslirios
string tag = "Imprimir Reporte"
integer x = 2263
integer y = 520
integer taborder = 520
integer weight = 400
fontcharset fontcharset = ansi!
string picturename = "\Desarrollo 12\Imagenes\Botones\Excel.png"
string disabledname = "\Desarrollo 12\Imagenes\Botones\Excel_bn.png"
end type

event pb_acepta::clicked;SetPointer(Arrow!)

Long	ll_Fila
String	ls_Archivo, ls_Ruta

RegistryGet( "HKEY_CURRENT_USER\Software\Microsoft\Windows\CurrentVersion\Explorer\Shell Folders", "Personal", RegString!, ls_Ruta)
ls_Archivo	= '\PalletExportacion_' + Mid(em_Desde.Text, 1, 2) + Mid(em_Desde.Text, 4, 2) + Mid(em_Desde.Text, 7, 4) + '.xls'

ll_Fila	=	dw_1.Retrieve(uo_SelCliente.Codigo, uo_SelPlanta.Codigo, Date(em_desde.Text), Date(em_Hasta.Text))

If ll_Fila = -1 Then
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
			   	"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ElseIf ll_Fila = 0 Then
	MessageBox( "No Existe información", "No existe información para este informe.", StopSign!, Ok!)
Else
	If dw_1.SaveAs(ls_Ruta + ls_Archivo, EXCEL8!, True) = -1 Then
		Messagebox('Error', 'Archivo no se pudo generar.', StopSign!, Ok!)
		Return
	Else
		Messagebox('Atencion', 'Archivo genrado en ruta:' + ls_Ruta + ls_Archivo, Information!, Ok!)
	End If
End If

SetPointer(Arrow!)
end event

type pb_salir from w_para_informes`pb_salir within w_info_existencia_loslirios
string tag = "Salir [Cerrar Ventana Activa]"
integer x = 2263
integer y = 804
integer taborder = 530
string powertiptext = "Salir [Cerrar Ventana Activa]"
end type

type st_6 from statictext within w_info_existencia_loslirios
integer x = 338
integer y = 460
integer width = 233
integer height = 64
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 553648127
string text = "Cliente"
boolean focusrectangle = false
end type

type gb_3 from groupbox within w_info_existencia_loslirios
integer x = 315
integer y = 412
integer width = 1609
integer height = 148
integer taborder = 10
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 553648127
end type

type st_4 from statictext within w_info_existencia_loslirios
integer x = 247
integer y = 404
integer width = 1728
integer height = 372
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_especie from statictext within w_info_existencia_loslirios
integer x = 343
integer y = 888
integer width = 238
integer height = 60
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 553648127
string text = "Desde"
boolean focusrectangle = false
end type

type st_nro2 from statictext within w_info_existencia_loslirios
integer x = 247
integer y = 776
integer width = 1728
integer height = 264
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_variedad from statictext within w_info_existencia_loslirios
integer x = 1221
integer y = 888
integer width = 279
integer height = 60
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 553648127
string text = "Hasta"
boolean focusrectangle = false
end type

type st_3 from statictext within w_info_existencia_loslirios
integer x = 338
integer y = 660
integer width = 233
integer height = 64
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 553648127
string text = "Planta"
boolean focusrectangle = false
end type

type uo_selcliente from uo_seleccion_clientesprod within w_info_existencia_loslirios
event destroy ( )
integer x = 635
integer y = 452
integer height = 84
integer taborder = 530
boolean bringtotop = true
end type

on uo_selcliente.destroy
call uo_seleccion_clientesprod::destroy
end on

type uo_selplanta from uo_seleccion_plantas within w_info_existencia_loslirios
event destroy ( )
integer x = 645
integer y = 568
integer taborder = 530
boolean bringtotop = true
end type

on uo_selplanta.destroy
call uo_seleccion_plantas::destroy
end on

type em_desde from editmask within w_info_existencia_loslirios
integer x = 645
integer y = 872
integer width = 434
integer height = 92
integer taborder = 540
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
boolean dropdowncalendar = true
end type

type em_hasta from editmask within w_info_existencia_loslirios
integer x = 1486
integer y = 872
integer width = 434
integer height = 92
integer taborder = 550
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
boolean dropdowncalendar = true
end type

type dw_1 from uo_dw within w_info_existencia_loslirios
boolean visible = false
integer x = 1330
integer width = 270
integer height = 192
integer taborder = 11
boolean bringtotop = true
string dataobject = "dw_gene_pallet_loslirios"
end type

