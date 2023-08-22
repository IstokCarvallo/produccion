$PBExportHeader$w_info_comercial_loslirios.srw
forward
global type w_info_comercial_loslirios from w_para_informes
end type
type st_6 from statictext within w_info_comercial_loslirios
end type
type gb_3 from groupbox within w_info_comercial_loslirios
end type
type st_4 from statictext within w_info_comercial_loslirios
end type
type st_3 from statictext within w_info_comercial_loslirios
end type
type uo_selcliente from uo_seleccion_clientesprod within w_info_comercial_loslirios
end type
type uo_selplanta from uo_seleccion_plantas within w_info_comercial_loslirios
end type
type dw_1 from uo_dw within w_info_comercial_loslirios
end type
end forward

global type w_info_comercial_loslirios from w_para_informes
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
st_3 st_3
uo_selcliente uo_selcliente
uo_selplanta uo_selplanta
dw_1 dw_1
end type
global w_info_comercial_loslirios w_info_comercial_loslirios

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

on w_info_comercial_loslirios.create
int iCurrent
call super::create
this.st_6=create st_6
this.gb_3=create gb_3
this.st_4=create st_4
this.st_3=create st_3
this.uo_selcliente=create uo_selcliente
this.uo_selplanta=create uo_selplanta
this.dw_1=create dw_1
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_6
this.Control[iCurrent+2]=this.gb_3
this.Control[iCurrent+3]=this.st_4
this.Control[iCurrent+4]=this.st_3
this.Control[iCurrent+5]=this.uo_selcliente
this.Control[iCurrent+6]=this.uo_selplanta
this.Control[iCurrent+7]=this.dw_1
end on

on w_info_comercial_loslirios.destroy
call super::destroy
destroy(this.st_6)
destroy(this.gb_3)
destroy(this.st_4)
destroy(this.st_3)
destroy(this.uo_selcliente)
destroy(this.uo_selplanta)
destroy(this.dw_1)
end on

event open;call super::open;Boolean	lb_Cerrar

If IsNull(uo_SelCliente.Codigo) Then lb_Cerrar = True
If IsNull(uo_SelPlanta.Codigo) Then lb_Cerrar = True

If lb_Cerrar Then
	Close(This)
Else	
	uo_SelCliente.Seleccion(False, False)
	uo_SelPlanta.Seleccion(True, False)
	
	uo_SelCliente.Codigo	= gi_CodExport
//	uo_SelPlanta.Codigo	= gi_CodPlanta
	uo_SelCliente.dw_Seleccion.Object.Codigo[1]	=	gi_CodExport
//	uo_SelPlanta.dw_Seleccion.Object.Codigo[1]	=	gi_CodPlanta
	
	dw_1.SetTransObject(Sqlca)
End If
end event

type st_computador from w_para_informes`st_computador within w_info_comercial_loslirios
end type

type st_usuario from w_para_informes`st_usuario within w_info_comercial_loslirios
end type

type st_temporada from w_para_informes`st_temporada within w_info_comercial_loslirios
end type

type p_logo from w_para_informes`p_logo within w_info_comercial_loslirios
end type

type st_titulo from w_para_informes`st_titulo within w_info_comercial_loslirios
integer width = 1728
integer height = 92
string text = "Archivo Comercial Los Lirios"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_comercial_loslirios
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
ls_Archivo	= '\FrutaComercial' + String(Today(), 'ddmmyyyy') + '.xls'

ll_Fila	=	dw_1.Retrieve(uo_SelCliente.Codigo, uo_SelPlanta.Codigo)

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

type pb_salir from w_para_informes`pb_salir within w_info_comercial_loslirios
string tag = "Salir [Cerrar Ventana Activa]"
integer x = 2263
integer y = 804
integer taborder = 530
string powertiptext = "Salir [Cerrar Ventana Activa]"
end type

type st_6 from statictext within w_info_comercial_loslirios
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

type gb_3 from groupbox within w_info_comercial_loslirios
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

type st_4 from statictext within w_info_comercial_loslirios
integer x = 247
integer y = 404
integer width = 1728
integer height = 612
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

type st_3 from statictext within w_info_comercial_loslirios
integer x = 338
integer y = 732
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

type uo_selcliente from uo_seleccion_clientesprod within w_info_comercial_loslirios
event destroy ( )
integer x = 645
integer y = 452
integer height = 84
integer taborder = 530
boolean bringtotop = true
end type

on uo_selcliente.destroy
call uo_seleccion_clientesprod::destroy
end on

type uo_selplanta from uo_seleccion_plantas within w_info_comercial_loslirios
event destroy ( )
integer x = 645
integer y = 640
integer taborder = 530
boolean bringtotop = true
end type

on uo_selplanta.destroy
call uo_seleccion_plantas::destroy
end on

type dw_1 from uo_dw within w_info_comercial_loslirios
boolean visible = false
integer x = 1330
integer width = 270
integer height = 192
integer taborder = 11
boolean bringtotop = true
string dataobject = "dw_gene_bins_loslirios"
end type

