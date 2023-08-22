$PBExportHeader$w_info_estiba_camaras.srw
forward
global type w_info_estiba_camaras from w_para_informes
end type
type gb_4 from groupbox within w_info_estiba_camaras
end type
type st_1 from statictext within w_info_estiba_camaras
end type
type st_6 from statictext within w_info_estiba_camaras
end type
type st_3 from statictext within w_info_estiba_camaras
end type
type st_8 from statictext within w_info_estiba_camaras
end type
type st_variedad from statictext within w_info_estiba_camaras
end type
type st_5 from statictext within w_info_estiba_camaras
end type
type cbx_calibre from checkbox within w_info_estiba_camaras
end type
type em_calidad from editmask within w_info_estiba_camaras
end type
type st_10 from statictext within w_info_estiba_camaras
end type
type st_2 from statictext within w_info_estiba_camaras
end type
type em_embalaje from editmask within w_info_estiba_camaras
end type
type cb_buscaembalaje from commandbutton within w_info_estiba_camaras
end type
type cbx_1 from checkbox within w_info_estiba_camaras
end type
type rb_listado from radiobutton within w_info_estiba_camaras
end type
type rb_plano from radiobutton within w_info_estiba_camaras
end type
type st_18 from statictext within w_info_estiba_camaras
end type
type rb_todos from radiobutton within w_info_estiba_camaras
end type
type rb_si from radiobutton within w_info_estiba_camaras
end type
type rb_resi from radiobutton within w_info_estiba_camaras
end type
type rb_no from radiobutton within w_info_estiba_camaras
end type
type rb_rechazadossag from radiobutton within w_info_estiba_camaras
end type
type rb_1 from radiobutton within w_info_estiba_camaras
end type
type dw_descripcion from datawindow within w_info_estiba_camaras
end type
type st_22 from statictext within w_info_estiba_camaras
end type
type st_48 from statictext within w_info_estiba_camaras
end type
type cbx_secuencia from checkbox within w_info_estiba_camaras
end type
type rb_2 from radiobutton within w_info_estiba_camaras
end type
type rb_3 from radiobutton within w_info_estiba_camaras
end type
type uo_selcondicion from uo_seleccion_condicion within w_info_estiba_camaras
end type
type st_4 from statictext within w_info_estiba_camaras
end type
type st_12 from statictext within w_info_estiba_camaras
end type
type st_13 from statictext within w_info_estiba_camaras
end type
type st_14 from statictext within w_info_estiba_camaras
end type
type st_15 from statictext within w_info_estiba_camaras
end type
type uo_selplanta from uo_seleccion_plantas within w_info_estiba_camaras
end type
type uo_selproductor from uo_seleccion_productor within w_info_estiba_camaras
end type
type uo_selpredio from uo_seleccion_prodpredio within w_info_estiba_camaras
end type
type uo_selcuartel from uo_seleccion_prodcuarteles within w_info_estiba_camaras
end type
type uo_selcliente from uo_seleccion_clientesprod within w_info_estiba_camaras
end type
type gb_6 from groupbox within w_info_estiba_camaras
end type
type gb_7 from groupbox within w_info_estiba_camaras
end type
type st_9 from statictext within w_info_estiba_camaras
end type
type st_7 from statictext within w_info_estiba_camaras
end type
type uo_selcamara from uo_seleccion_camarasbode within w_info_estiba_camaras
end type
type uo_selespecie from uo_seleccion_especie within w_info_estiba_camaras
end type
type uo_selvariedad from uo_seleccion_variedad within w_info_estiba_camaras
end type
type uo_seldestino from uo_seleccion_destinos within w_info_estiba_camaras
end type
end forward

global type w_info_estiba_camaras from w_para_informes
integer x = 14
integer y = 32
integer width = 4485
integer height = 2104
string title = "Informe Estiba Cámaras"
boolean minbox = false
boolean maxbox = false
boolean resizable = false
windowtype windowtype = response!
string icon = "F:\Desarrollo\Producción\FrutaProcesada\Producc.ico"
gb_4 gb_4
st_1 st_1
st_6 st_6
st_3 st_3
st_8 st_8
st_variedad st_variedad
st_5 st_5
cbx_calibre cbx_calibre
em_calidad em_calidad
st_10 st_10
st_2 st_2
em_embalaje em_embalaje
cb_buscaembalaje cb_buscaembalaje
cbx_1 cbx_1
rb_listado rb_listado
rb_plano rb_plano
st_18 st_18
rb_todos rb_todos
rb_si rb_si
rb_resi rb_resi
rb_no rb_no
rb_rechazadossag rb_rechazadossag
rb_1 rb_1
dw_descripcion dw_descripcion
st_22 st_22
st_48 st_48
cbx_secuencia cbx_secuencia
rb_2 rb_2
rb_3 rb_3
uo_selcondicion uo_selcondicion
st_4 st_4
st_12 st_12
st_13 st_13
st_14 st_14
st_15 st_15
uo_selplanta uo_selplanta
uo_selproductor uo_selproductor
uo_selpredio uo_selpredio
uo_selcuartel uo_selcuartel
uo_selcliente uo_selcliente
gb_6 gb_6
gb_7 gb_7
st_9 st_9
st_7 st_7
uo_selcamara uo_selcamara
uo_selespecie uo_selespecie
uo_selvariedad uo_selvariedad
uo_seldestino uo_seldestino
end type
global w_info_estiba_camaras w_info_estiba_camaras

type variables
str_busqueda istr_busq
str_mant istr_mant

Long	il_secuencia

DataWindowChild	dwc_descripcion

String 	is_descripcion

uo_calibre	iuo_calibre
end variables

forward prototypes
public function boolean existedescripcion (integer ai_especie, integer ai_destino, integer ai_secuencia)
end prototypes

public function boolean existedescripcion (integer ai_especie, integer ai_destino, integer ai_secuencia);Integer	li_Cliente, li_Planta
Long		ll_Numero

SELECT dsag_descrip
INTO :is_descripcion
FROM dbo.destinossag
WHERE :ai_destino in (-1,dest_codigo)
AND	:ai_especie in (-1,espe_codigo)
AND	dsag_secuen =  :ai_secuencia;

IF is_descripcion = '' THEN
	MessageBox("Cuidado","destinossag No Existe")
	RETURN False
END IF
RETURN True	
end function

on w_info_estiba_camaras.create
int iCurrent
call super::create
this.gb_4=create gb_4
this.st_1=create st_1
this.st_6=create st_6
this.st_3=create st_3
this.st_8=create st_8
this.st_variedad=create st_variedad
this.st_5=create st_5
this.cbx_calibre=create cbx_calibre
this.em_calidad=create em_calidad
this.st_10=create st_10
this.st_2=create st_2
this.em_embalaje=create em_embalaje
this.cb_buscaembalaje=create cb_buscaembalaje
this.cbx_1=create cbx_1
this.rb_listado=create rb_listado
this.rb_plano=create rb_plano
this.st_18=create st_18
this.rb_todos=create rb_todos
this.rb_si=create rb_si
this.rb_resi=create rb_resi
this.rb_no=create rb_no
this.rb_rechazadossag=create rb_rechazadossag
this.rb_1=create rb_1
this.dw_descripcion=create dw_descripcion
this.st_22=create st_22
this.st_48=create st_48
this.cbx_secuencia=create cbx_secuencia
this.rb_2=create rb_2
this.rb_3=create rb_3
this.uo_selcondicion=create uo_selcondicion
this.st_4=create st_4
this.st_12=create st_12
this.st_13=create st_13
this.st_14=create st_14
this.st_15=create st_15
this.uo_selplanta=create uo_selplanta
this.uo_selproductor=create uo_selproductor
this.uo_selpredio=create uo_selpredio
this.uo_selcuartel=create uo_selcuartel
this.uo_selcliente=create uo_selcliente
this.gb_6=create gb_6
this.gb_7=create gb_7
this.st_9=create st_9
this.st_7=create st_7
this.uo_selcamara=create uo_selcamara
this.uo_selespecie=create uo_selespecie
this.uo_selvariedad=create uo_selvariedad
this.uo_seldestino=create uo_seldestino
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.gb_4
this.Control[iCurrent+2]=this.st_1
this.Control[iCurrent+3]=this.st_6
this.Control[iCurrent+4]=this.st_3
this.Control[iCurrent+5]=this.st_8
this.Control[iCurrent+6]=this.st_variedad
this.Control[iCurrent+7]=this.st_5
this.Control[iCurrent+8]=this.cbx_calibre
this.Control[iCurrent+9]=this.em_calidad
this.Control[iCurrent+10]=this.st_10
this.Control[iCurrent+11]=this.st_2
this.Control[iCurrent+12]=this.em_embalaje
this.Control[iCurrent+13]=this.cb_buscaembalaje
this.Control[iCurrent+14]=this.cbx_1
this.Control[iCurrent+15]=this.rb_listado
this.Control[iCurrent+16]=this.rb_plano
this.Control[iCurrent+17]=this.st_18
this.Control[iCurrent+18]=this.rb_todos
this.Control[iCurrent+19]=this.rb_si
this.Control[iCurrent+20]=this.rb_resi
this.Control[iCurrent+21]=this.rb_no
this.Control[iCurrent+22]=this.rb_rechazadossag
this.Control[iCurrent+23]=this.rb_1
this.Control[iCurrent+24]=this.dw_descripcion
this.Control[iCurrent+25]=this.st_22
this.Control[iCurrent+26]=this.st_48
this.Control[iCurrent+27]=this.cbx_secuencia
this.Control[iCurrent+28]=this.rb_2
this.Control[iCurrent+29]=this.rb_3
this.Control[iCurrent+30]=this.uo_selcondicion
this.Control[iCurrent+31]=this.st_4
this.Control[iCurrent+32]=this.st_12
this.Control[iCurrent+33]=this.st_13
this.Control[iCurrent+34]=this.st_14
this.Control[iCurrent+35]=this.st_15
this.Control[iCurrent+36]=this.uo_selplanta
this.Control[iCurrent+37]=this.uo_selproductor
this.Control[iCurrent+38]=this.uo_selpredio
this.Control[iCurrent+39]=this.uo_selcuartel
this.Control[iCurrent+40]=this.uo_selcliente
this.Control[iCurrent+41]=this.gb_6
this.Control[iCurrent+42]=this.gb_7
this.Control[iCurrent+43]=this.st_9
this.Control[iCurrent+44]=this.st_7
this.Control[iCurrent+45]=this.uo_selcamara
this.Control[iCurrent+46]=this.uo_selespecie
this.Control[iCurrent+47]=this.uo_selvariedad
this.Control[iCurrent+48]=this.uo_seldestino
end on

on w_info_estiba_camaras.destroy
call super::destroy
destroy(this.gb_4)
destroy(this.st_1)
destroy(this.st_6)
destroy(this.st_3)
destroy(this.st_8)
destroy(this.st_variedad)
destroy(this.st_5)
destroy(this.cbx_calibre)
destroy(this.em_calidad)
destroy(this.st_10)
destroy(this.st_2)
destroy(this.em_embalaje)
destroy(this.cb_buscaembalaje)
destroy(this.cbx_1)
destroy(this.rb_listado)
destroy(this.rb_plano)
destroy(this.st_18)
destroy(this.rb_todos)
destroy(this.rb_si)
destroy(this.rb_resi)
destroy(this.rb_no)
destroy(this.rb_rechazadossag)
destroy(this.rb_1)
destroy(this.dw_descripcion)
destroy(this.st_22)
destroy(this.st_48)
destroy(this.cbx_secuencia)
destroy(this.rb_2)
destroy(this.rb_3)
destroy(this.uo_selcondicion)
destroy(this.st_4)
destroy(this.st_12)
destroy(this.st_13)
destroy(this.st_14)
destroy(this.st_15)
destroy(this.uo_selplanta)
destroy(this.uo_selproductor)
destroy(this.uo_selpredio)
destroy(this.uo_selcuartel)
destroy(this.uo_selcliente)
destroy(this.gb_6)
destroy(this.gb_7)
destroy(this.st_9)
destroy(this.st_7)
destroy(this.uo_selcamara)
destroy(this.uo_selespecie)
destroy(this.uo_selvariedad)
destroy(this.uo_seldestino)
end on

event open;call super::open;Boolean	lb_Cerrar

//Descripcion destino
dw_descripcion.GetChild("dsag_secuen", dwc_descripcion)
dwc_descripcion.SetTransObject(Sqlca)
dwc_descripcion.Retrieve(-1,-1)			//especie
dw_descripcion.InsertRow(1)

IF IsNull(uo_SelCliente.Codigo) THEN lb_Cerrar	=	True
IF IsNull(uo_SelPlanta.Codigo) THEN lb_Cerrar	=	True
IF IsNull(uo_SelCamara.Codigo) THEN lb_Cerrar	=	True
IF IsNull(uo_SelCondicion.Codigo) THEN lb_Cerrar	=	True
IF IsNull(uo_SelEspecie.Codigo) THEN lb_Cerrar	=	True
IF IsNull(uo_SelVariedad.Codigo) THEN lb_Cerrar	=	True
IF IsNull(uo_SelDestino.Codigo) THEN lb_Cerrar	=	True
IF IsNull(uo_SelProductor.Codigo) THEN lb_Cerrar	=	True
IF IsNull(uo_SelPredio.Codigo) THEN lb_Cerrar	=	True
IF IsNull(uo_SelCuartel.Codigo) THEN lb_Cerrar	=	True

If lb_Cerrar Then
	Close(This)
Else
	uo_SelCliente.Seleccion(True, False)
	uo_SelPlanta.Seleccion(False, False)
	uo_SelCamara.Seleccion(True, False)
	uo_selCondicion.Seleccion(True, True)
	uo_SelEspecie.Seleccion(True, False)
	uo_SelVariedad.Seleccion(True, False)
	uo_SelDestino.Seleccion(True, True)
	uo_SelProductor.Seleccion(True, True)
	uo_SelPredio.Seleccion(True, True)
	uo_SelCuartel.Seleccion(True, True)
	
	uo_SelPlanta.Filtra(1)
	uo_SelPlanta.Inicia(gi_CodPlanta)
	uo_SelCamara.Filtra(gi_CodPlanta)
	uo_SelProductor.Filtra(-1)
	
	iuo_Calibre  =	Create uo_Calibre
	
	istr_mant.argumento[6]	=	'*'                // embalaje
	istr_mant.argumento[7]	=	'*'               // calibre
	istr_mant.argumento[11]	=	'0'			// Inspección
	istr_mant.argumento[20]	=	'-9'
End If
end event

event resize;//
end event

type pb_excel from w_para_informes`pb_excel within w_info_estiba_camaras
integer x = 3890
integer y = 900
end type

type st_computador from w_para_informes`st_computador within w_info_estiba_camaras
end type

type st_usuario from w_para_informes`st_usuario within w_info_estiba_camaras
end type

type st_temporada from w_para_informes`st_temporada within w_info_estiba_camaras
end type

type p_logo from w_para_informes`p_logo within w_info_estiba_camaras
integer width = 1051
integer height = 264
end type

type st_titulo from w_para_informes`st_titulo within w_info_estiba_camaras
integer x = 283
integer y = 280
integer width = 3392
string text = "Informe Estiba Cámaras"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_estiba_camaras
string tag = "Imprimir Reporte"
integer x = 3899
integer y = 1272
integer taborder = 100
end type

event pb_acepta::clicked;Integer	fila, li_inspeccion
String		ls_embalaje, ls_calibre, ls_data_object

If cbx_secuencia.Checked Then
	il_secuencia = -1
Else	
	il_secuencia = dw_descripcion.Object.dsag_secuen[1]
End If	

ls_embalaje		=	istr_mant.argumento[6]
ls_calibre		=	istr_mant.argumento[7]
li_inspeccion	=  Integer(istr_mant.argumento[11])


//selecciona el tipo de Informe
If rb_plano.checked Then
	istr_info.titulo	= 'Informe '+rb_plano.text+' Cámaras'
	ls_data_object = "dw_info_estiba_camaras"
Else
	istr_info.titulo	= 'Informe '+rb_listado.text+' Cámaras'
   	ls_data_object = "dw_info_listado_estiba_camara"
End If	

SetPointer(HourGlass!)
OpenWithParm(vinf, istr_info)
vinf.dw_1.DataObject = ls_data_object

vinf.dw_1.SetTransObject(sqlca)

If rb_plano.checked = TRUE Then
	fila	=	vinf.dw_1.Retrieve(uo_SelCliente.Codigo, uo_SelPlanta.Codigo, uo_SelCamara.Codigo, uo_SelEspecie.Codigo, uo_SelVariedad.Codigo,&
										 ls_embalaje, ls_calibre, uo_SelCondicion.codigo, uo_SelDestino.Codigo, li_inspeccion, il_secuencia,&
										 uo_SelProductor.Codigo,uo_SelPredio.Codigo,uo_SelCuartel.Codigo)
Else
	fila	=	vinf.dw_1.Retrieve(uo_SelCliente.Codigo, uo_SelPlanta.Codigo, uo_SelCamara.Codigo, uo_SelEspecie.Codigo, uo_SelVariedad.Codigo,&
									 ls_embalaje, ls_calibre, uo_SelCondicion.codigo, uo_SelDestino.Codigo, li_inspeccion, il_secuencia)
End If	

If fila = -1 Then
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ElseIf fila = 0 Then
	MessageBox( "No Existe información", "No existe información para este informe.", &
					StopSign!, Ok!)
Else
	If rb_listado.Checked Then
		If rb_2.Checked Then
			vinf.dw_1.SetSort("paen_fecemb asc, clie_codigo asc")
			vinf.dw_1.Sort( )
		Else
			vinf.dw_1.SetSort("clie_codigo asc,pafr_copack asc,espe_nombre asc, vari_nombre asc,emba_codigo asc,pafr_calibr asc,paen_fecemb asc")
			vinf.dw_1.Sort( )
		End If	
	End If	
	
	F_Membrete(vinf.dw_1)
	vinf.dw_1.ModIfy("planta.Text = 'Planta "+uo_SelPlanta.Nombre+ "'")
	
	If rb_todos.Checked Then
		vinf.dw_1.ModIfy("t_sag.Text = '" + 'Todos' + "'")
	ElseIf rb_si.Checked Then
		vinf.dw_1.ModIfy("t_sag.Text = '" + 'Inspeccionados' + "'")
	ElseIf rb_resi.Checked Then
		vinf.dw_1.ModIfy("t_sag.Text = '" + 'ReInspeccionados' + "'")
	ElseIf rb_no.Checked Then
		vinf.dw_1.ModIfy("t_sag.Text = '" + 'No Inspeccionados' + "'")
	ElseIf rb_rechazadossag.Checked Then
		vinf.dw_1.ModIfy("t_sag.Text = '" + 'Rechazados' + "'")
	ElseIf rb_1.Checked Then
		vinf.dw_1.ModIfy("t_sag.Text = '" + 'Pendientes' + "'")
	End If
	
	If gs_Ambiente <> 'Windows' Then F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
End If

SetPointer(Arrow!)

end event

type pb_salir from w_para_informes`pb_salir within w_info_estiba_camaras
string tag = "Salir [Cerrar Ventana Activa]"
integer x = 3899
integer y = 1524
integer taborder = 110
string powertiptext = "Salir [Cerrar Ventana Activa]"
end type

type gb_4 from groupbox within w_info_estiba_camaras
integer x = 311
integer y = 1720
integer width = 1637
integer height = 180
integer taborder = 90
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Tipo Informe"
end type

type st_1 from statictext within w_info_estiba_camaras
integer x = 421
integer y = 624
integer width = 306
integer height = 76
boolean bringtotop = true
integer textsize = -9
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

type st_6 from statictext within w_info_estiba_camaras
integer x = 425
integer y = 520
integer width = 306
integer height = 64
boolean bringtotop = true
integer textsize = -9
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

type st_3 from statictext within w_info_estiba_camaras
integer x = 2057
integer y = 520
integer width = 242
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

type st_8 from statictext within w_info_estiba_camaras
integer x = 421
integer y = 812
integer width = 306
integer height = 64
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Cámara"
boolean focusrectangle = false
end type

type st_variedad from statictext within w_info_estiba_camaras
integer x = 2057
integer y = 704
integer width = 279
integer height = 96
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

type st_5 from statictext within w_info_estiba_camaras
integer x = 1979
integer y = 408
integer width = 1696
integer height = 688
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 16711680
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type cbx_calibre from checkbox within w_info_estiba_camaras
integer x = 2633
integer y = 976
integer width = 270
integer height = 80
integer taborder = 80
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
	em_calidad.Text			=	''
	em_calidad.Enabled		=	False
	istr_mant.argumento[7]	=	'*'
ELSE
	em_calidad.Enabled		=	True
	em_calidad.SetFocus()
END IF


end event

type em_calidad from editmask within w_info_estiba_camaras
integer x = 2350
integer y = 968
integer width = 233
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
boolean enabled = false
alignment alignment = center!
textcase textcase = upper!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = stringmask!
end type

event modified;String		ls_calibre

ls_calibre	=	This.Text

IF This.Text <> '' THEN
	ls_calibre	=	This.Text
	
	IF NOT iuo_calibre.Existe(uo_SelEspecie.Codigo, uo_SelVariedad.Codigo, ls_calibre,True,sqlca) THEN
		em_calidad.Text = ''
		This.SetFocus()
		Return 1
	ELSE	
		istr_mant.argumento[7]	 = iuo_calibre.calibre
		em_calidad.Text			 = iuo_calibre.calibre
		Return 1
	END IF	
END IF	



	
	
end event

type st_10 from statictext within w_info_estiba_camaras
integer x = 2057
integer y = 980
integer width = 256
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
string text = "Calibre"
boolean focusrectangle = false
end type

type st_2 from statictext within w_info_estiba_camaras
integer x = 2057
integer y = 836
integer width = 288
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
string text = "Embalaje"
boolean focusrectangle = false
end type

type em_embalaje from editmask within w_info_estiba_camaras
integer x = 2350
integer y = 824
integer width = 242
integer height = 96
integer taborder = 70
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
boolean enabled = false
alignment alignment = center!
textcase textcase = upper!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = stringmask!
end type

event modified;Integer		li_cliente
String		ls_embalaje, ls_Nombre

li_cliente	=	Integer(istr_mant.argumento[1]) // Cliente
ls_embalaje	=	This.Text

SELECT	emba_nombre
	INTO	:ls_Nombre
	FROM	dbo.embalajesprod
	WHERE	clie_codigo	=	:li_cliente
	AND	emba_codigo	=	:ls_embalaje;
	
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de tabla Embalajes")
	This.SetFocus()
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención", "Código de Embalaje no ha sido Definido.~r~r" + &
		"Ingrese o seleccione otro Código.")
	This.SetFocus()
ELSE
	istr_mant.argumento[6]	=	ls_embalaje
END IF
end event

type cb_buscaembalaje from commandbutton within w_info_estiba_camaras
integer x = 2633
integer y = 832
integer width = 96
integer height = 84
integer taborder = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
boolean enabled = false
string text = "..."
end type

event clicked;Str_busqueda	lstr_busq

lstr_busq.argum[1]	=	istr_mant.argumento[1] // Cliente

OpenWithParm(w_busc_embalajesprod, lstr_busq)

lstr_busq	       = Message.PowerObjectParm

IF lstr_busq.argum[2] = "" THEN
	em_embalaje.SetFocus()
ELSE
	em_embalaje.Text			=	lstr_busq.argum[2]
	istr_mant.argumento[6]	=	lstr_busq.argum[2]
END IF
end event

type cbx_1 from checkbox within w_info_estiba_camaras
integer x = 2761
integer y = 832
integer width = 270
integer height = 80
integer taborder = 80
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
	em_embalaje.Text			=	''
	em_embalaje.Enabled		=	False
	istr_mant.argumento[6]	=	'*'
	cb_buscaembalaje.Enabled = False;
ELSE
	em_embalaje.Enabled		=	True
	em_embalaje.SetFocus()
	cb_buscaembalaje.Enabled = True;
END IF

end event

type rb_listado from radiobutton within w_info_estiba_camaras
integer x = 434
integer y = 1792
integer width = 640
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
string text = "Listado Estiba"
end type

event clicked;IF This.Checked THEN
	rb_2.Enabled = True
	rb_3.Enabled = True
END IF	
end event

type rb_plano from radiobutton within w_info_estiba_camaras
integer x = 1088
integer y = 1784
integer width = 567
integer height = 96
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Plano Estiba"
boolean checked = true
end type

event clicked;IF This.Checked THEN
	rb_2.Enabled = False
	rb_3.Enabled = True
END IF	
end event

type st_18 from statictext within w_info_estiba_camaras
integer x = 421
integer y = 976
integer width = 306
integer height = 80
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Condición"
boolean focusrectangle = false
end type

type rb_todos from radiobutton within w_info_estiba_camaras
integer x = 416
integer y = 1172
integer width = 279
integer height = 76
boolean bringtotop = true
integer textsize = -8
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

event clicked;istr_mant.argumento[11]	=	'0'

dw_descripcion.Enabled										=	False
dw_descripcion.Object.dsag_secuen.BackGround.Color	=	RGB(166,180,210)
cbx_secuencia.Enabled 										=  False
cbx_secuencia.Checked 										=  True
end event

type rb_si from radiobutton within w_info_estiba_camaras
integer x = 882
integer y = 1172
integer width = 530
integer height = 76
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Inspeccionados"
end type

event clicked;istr_mant.argumento[11]	=	'1'

dw_descripcion.Enabled										=	False
dw_descripcion.Object.dsag_secuen.BackGround.Color	=	RGB(166,180,210)
end event

type rb_resi from radiobutton within w_info_estiba_camaras
integer x = 1495
integer y = 1172
integer width = 375
integer height = 76
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "ReInspec."
end type

event clicked;istr_mant.argumento[11]	=	'4'

dw_descripcion.Enabled										=	False
dw_descripcion.Object.dsag_secuen.BackGround.Color	=	RGB(166,180,210)
cbx_secuencia.Enabled										=	False
cbx_secuencia.Checked										=	True
end event

type rb_no from radiobutton within w_info_estiba_camaras
integer x = 416
integer y = 1244
integer width = 416
integer height = 80
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "No Inspec."
end type

event clicked;istr_mant.argumento[11]	=	'2'

dw_descripcion.Enabled										=	False
dw_descripcion.Object.dsag_secuen.BackGround.Color	=	RGB(166,180,210)
cbx_secuencia.Enabled										=	False
cbx_secuencia.Checked										=	True
end event

type rb_rechazadossag from radiobutton within w_info_estiba_camaras
integer x = 882
integer y = 1244
integer width = 411
integer height = 80
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Rechazados"
end type

event clicked;istr_mant.argumento[11]	=	'3'

dw_descripcion.Enabled										=	False
dw_descripcion.Object.dsag_secuen.BackGround.Color	=	RGB(166,180,210)
cbx_secuencia.Enabled										=	False
cbx_secuencia.Checked										=	True
end event

type rb_1 from radiobutton within w_info_estiba_camaras
integer x = 1495
integer y = 1240
integer width = 393
integer height = 80
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Pendientes"
end type

event clicked;istr_mant.argumento[11]	=	'5'

dw_descripcion.Enabled										=	False
dw_descripcion.Object.dsag_secuen.BackGround.Color	=	RGB(166,180,210)
cbx_secuencia.Enabled										=	False
cbx_secuencia.Checked										=	True
end event

type dw_descripcion from datawindow within w_info_estiba_camaras
integer x = 690
integer y = 1508
integer width = 855
integer height = 92
integer taborder = 70
boolean bringtotop = true
boolean enabled = false
string title = "none"
string dataobject = "dddw_destinosag"
boolean border = false
boolean livescroll = true
end type

event itemchanged;IF ExisteDescripcion(uo_SelEspecie.Codigo, uo_SelDestino.Codigo,Integer(data)) THEN
	il_secuencia = Integer(data)
END IF
end event

type st_22 from statictext within w_info_estiba_camaras
integer x = 347
integer y = 1528
integer width = 329
integer height = 68
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Secuencia"
boolean focusrectangle = false
end type

type st_48 from statictext within w_info_estiba_camaras
integer x = 343
integer y = 1416
integer width = 265
integer height = 68
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Destino"
boolean focusrectangle = false
end type

type cbx_secuencia from checkbox within w_info_estiba_camaras
integer x = 1591
integer y = 1524
integer width = 256
integer height = 64
integer taborder = 230
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
boolean checked = true
end type

event clicked;IF This.Checked THEN
	dw_descripcion.Reset()
	dw_descripcion.GetChild("dsag_secuen", dwc_descripcion)
	dwc_descripcion.SetTransObject(Sqlca)
	dwc_descripcion.Retrieve(Integer(istr_mant.argumento[4]),-1)			//especie
	dw_descripcion.InsertRow(0)
	dw_descripcion.Enabled										=	False
	dw_descripcion.Object.dsag_secuen.BackGround.Color	=	RGB(166,180,210)
	il_secuencia	=	-1
ELSE
	dw_descripcion.Enabled										=	True
	dw_descripcion.Object.dsag_secuen.BackGround.Color	=	RGB(255, 255, 255)
	
	dw_descripcion.SetFocus()
	dw_descripcion.Reset()
	dwc_descripcion.Retrieve(0)
	dw_descripcion.InsertRow(0)
END IF


end event

type rb_2 from radiobutton within w_info_estiba_camaras
integer x = 2181
integer y = 1792
integer width = 443
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
string text = "Fecha"
boolean checked = true
end type

type rb_3 from radiobutton within w_info_estiba_camaras
integer x = 3049
integer y = 1784
integer width = 370
integer height = 96
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
string text = "Cliente"
end type

type uo_selcondicion from uo_seleccion_condicion within w_info_estiba_camaras
event destroy ( )
integer x = 750
integer y = 896
integer height = 184
integer taborder = 280
end type

on uo_selcondicion.destroy
call uo_seleccion_condicion::destroy
end on

type st_4 from statictext within w_info_estiba_camaras
integer x = 283
integer y = 408
integer width = 1696
integer height = 688
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 16711680
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_12 from statictext within w_info_estiba_camaras
integer x = 1979
integer y = 1100
integer width = 1696
integer height = 592
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 16711680
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_13 from statictext within w_info_estiba_camaras
integer x = 2053
integer y = 1208
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

type st_14 from statictext within w_info_estiba_camaras
integer x = 2053
integer y = 1388
integer width = 283
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
string text = "Predio"
boolean focusrectangle = false
end type

type st_15 from statictext within w_info_estiba_camaras
integer x = 2053
integer y = 1568
integer width = 288
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
string text = "Cuartel"
boolean focusrectangle = false
end type

type uo_selplanta from uo_seleccion_plantas within w_info_estiba_camaras
integer x = 750
integer y = 612
integer height = 92
integer taborder = 70
boolean bringtotop = true
end type

on uo_selplanta.destroy
call uo_seleccion_plantas::destroy
end on

event ue_cambio;call super::ue_cambio;If IsNull(This.Codigo) Then Return

Choose Case This.Codigo
	Case -1, -9
		uo_SelCamara.Filtra(-1)
		uo_SelCamara.Todos(True)
		
	Case Else
		uo_SelCamara.Filtra(This.Codigo)
		
End Choose
end event

type uo_selproductor from uo_seleccion_productor within w_info_estiba_camaras
integer x = 2446
integer y = 1112
integer taborder = 20
boolean bringtotop = true
end type

on uo_selproductor.destroy
call uo_seleccion_productor::destroy
end on

event ue_cambio;call super::ue_cambio;if IsNull(This.Codigo) Then Return

Choose Case This.Codigo
	Case -1, -9
		uo_SelPredio.Filtra(-1)
		uo_SelCuartel.Filtra(-1, -1)
		uo_SelPredio.Todos(True)
		uo_SelCuartel.Todos(True)
		
	Case Else
		uo_SelPredio.Filtra(This.Codigo)
		uo_SelCuartel.Filtra(This.Codigo, -1)
		
End Choose
end event

type uo_selpredio from uo_seleccion_prodpredio within w_info_estiba_camaras
event destroy ( )
integer x = 2446
integer y = 1292
integer taborder = 20
boolean bringtotop = true
end type

on uo_selpredio.destroy
call uo_seleccion_prodpredio::destroy
end on

event ue_cambio;call super::ue_cambio;if IsNull(This.Codigo) Then Return

Choose Case This.Codigo
	Case -1, -9
		uo_SelCuartel.Filtra(-1, -1)
		uo_SelCuartel.Todos(True)
		
	Case Else
		uo_SelCuartel.Filtra(uo_SelProductor.Codigo, This.Codigo)
		
End Choose
end event

type uo_selcuartel from uo_seleccion_prodcuarteles within w_info_estiba_camaras
event destroy ( )
integer x = 2446
integer y = 1472
integer taborder = 80
boolean bringtotop = true
end type

on uo_selcuartel.destroy
call uo_seleccion_prodcuarteles::destroy
end on

type uo_selcliente from uo_seleccion_clientesprod within w_info_estiba_camaras
integer x = 750
integer y = 424
integer taborder = 80
boolean bringtotop = true
end type

on uo_selcliente.destroy
call uo_seleccion_clientesprod::destroy
end on

type gb_6 from groupbox within w_info_estiba_camaras
integer x = 2007
integer y = 1720
integer width = 1637
integer height = 180
integer taborder = 100
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Ordenado Por"
end type

type gb_7 from groupbox within w_info_estiba_camaras
integer x = 311
integer y = 1108
integer width = 1637
integer height = 528
integer taborder = 310
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "SAG"
end type

type st_9 from statictext within w_info_estiba_camaras
integer x = 283
integer y = 1100
integer width = 1696
integer height = 592
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

type st_7 from statictext within w_info_estiba_camaras
integer x = 283
integer y = 1692
integer width = 3392
integer height = 236
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 16711680
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type uo_selcamara from uo_seleccion_camarasbode within w_info_estiba_camaras
integer x = 750
integer y = 724
integer taborder = 80
boolean bringtotop = true
end type

on uo_selcamara.destroy
call uo_seleccion_camarasbode::destroy
end on

event ue_cambio;call super::ue_cambio;If IsNull(This.Codigo) Then Return

If This.Codigo = 0 Then 
	rb_plano.enabled	=	False
	rb_listado.checked	=	True
else	
   rb_plano.enabled	=	True
end if 	

end event

type uo_selespecie from uo_seleccion_especie within w_info_estiba_camaras
event destroy ( )
integer x = 2350
integer y = 420
integer taborder = 80
boolean bringtotop = true
end type

on uo_selespecie.destroy
call uo_seleccion_especie::destroy
end on

event ue_cambio;call super::ue_cambio;If IsNull(This.Codigo) Then Return

Choose Case This.Codigo
	Case -1, -9
		dw_descripcion.GetChild("dsag_secuen", dwc_descripcion)
		dwc_descripcion.SetTransObject(Sqlca)
		dwc_descripcion.Retrieve(-1,-1)			//especie
		dw_descripcion.InsertRow(1)	
		uo_SelVariedad.Filtra(-1)
	Case Else
		//Descripcion destino
		dw_descripcion.GetChild("dsag_secuen", dwc_descripcion)
		dwc_descripcion.SetTransObject(Sqlca)
		dwc_descripcion.Retrieve(This.Codigo,-1)			//especie
		dw_descripcion.InsertRow(1)
		
		uo_SelVariedad.Filtra(This.Codigo)
		
End Choose
end event

type uo_selvariedad from uo_seleccion_variedad within w_info_estiba_camaras
event destroy ( )
integer x = 2350
integer y = 608
integer taborder = 80
boolean bringtotop = true
end type

on uo_selvariedad.destroy
call uo_seleccion_variedad::destroy
end on

type uo_seldestino from uo_seleccion_destinos within w_info_estiba_camaras
integer x = 690
integer y = 1320
integer taborder = 240
boolean bringtotop = true
end type

on uo_seldestino.destroy
call uo_seleccion_destinos::destroy
end on

event ue_cambio;call super::ue_cambio;If IsNull(This.Codigo) Then Return

Choose Case This.Codigo
	Case -1, -9
		dw_descripcion.Enabled											=	False
		dw_descripcion.Object.dsag_secuen.BackGround.Color	=	553648127


	Case Else
		//Descripcion destino
		dw_descripcion.Enabled											=	True
		dw_descripcion.Object.dsag_secuen.BackGround.Color	=	RGB(255,255,255)
		
		dw_descripcion.GetChild("dsag_secuen", dwc_descripcion)
		dwc_descripcion.SetTransObject(Sqlca)
		dwc_descripcion.Retrieve(uo_SelEspecie.Codigo, This.Codigo)
		dw_descripcion.InsertRow(1)
		cbx_secuencia.Enabled 	=  True
		cbx_secuencia.Checked 	= True
		il_secuencia					=	-1
		
End Choose

end event

