$PBExportHeader$w_info_resolucionlotesdestino.srw
$PBExportComments$Ventana de consulta Lotes con destino
forward
global type w_info_resolucionlotesdestino from w_para_informes
end type
type gb_3 from groupbox within w_info_resolucionlotesdestino
end type
type st_14 from statictext within w_info_resolucionlotesdestino
end type
type st_44 from statictext within w_info_resolucionlotesdestino
end type
type st_1 from statictext within w_info_resolucionlotesdestino
end type
type st_2 from statictext within w_info_resolucionlotesdestino
end type
type st_3 from statictext within w_info_resolucionlotesdestino
end type
type uo_selvariedad from uo_seleccion_variedad_mod within w_info_resolucionlotesdestino
end type
type uo_selzonas from uo_seleccion_zonas_mod within w_info_resolucionlotesdestino
end type
type uo_selproductor from uo_seleccion_productor_zonas_mod within w_info_resolucionlotesdestino
end type
type st_4 from statictext within w_info_resolucionlotesdestino
end type
type st_5 from statictext within w_info_resolucionlotesdestino
end type
type st_7 from statictext within w_info_resolucionlotesdestino
end type
type uo_selnaves from uo_seleccion_naves_mod within w_info_resolucionlotesdestino
end type
type uo_seltipo from uo_seleccion_tipotransporte_mod within w_info_resolucionlotesdestino
end type
type cbx_todocon from checkbox within w_info_resolucionlotesdestino
end type
type em_contenedor from editmask within w_info_resolucionlotesdestino
end type
type cbx_conscon from checkbox within w_info_resolucionlotesdestino
end type
type st_8 from statictext within w_info_resolucionlotesdestino
end type
type uo_selrecibidor from uo_seleccion_recibidor_mod within w_info_resolucionlotesdestino
end type
type uo_selespecie from uo_seleccion_especie_mod within w_info_resolucionlotesdestino
end type
type st_10 from statictext within w_info_resolucionlotesdestino
end type
type rb_embalada from radiobutton within w_info_resolucionlotesdestino
end type
type rb_embarcada from radiobutton within w_info_resolucionlotesdestino
end type
type uo_selembalajes from uo_seleccion_embalajesprod_mod within w_info_resolucionlotesdestino
end type
type uo_selplanta from uo_seleccion_frigorifico_mod within w_info_resolucionlotesdestino
end type
type st_6 from statictext within w_info_resolucionlotesdestino
end type
type st_9 from statictext within w_info_resolucionlotesdestino
end type
type st_11 from statictext within w_info_resolucionlotesdestino
end type
type ddlb_condicion from dropdownlistbox within w_info_resolucionlotesdestino
end type
type ddlb_resolucion from dropdownlistbox within w_info_resolucionlotesdestino
end type
type st_12 from statictext within w_info_resolucionlotesdestino
end type
type st_13 from statictext within w_info_resolucionlotesdestino
end type
type cbx_todores from checkbox within w_info_resolucionlotesdestino
end type
type cbx_todocat from checkbox within w_info_resolucionlotesdestino
end type
type em_desdee from editmask within w_info_resolucionlotesdestino
end type
type em_hastae from editmask within w_info_resolucionlotesdestino
end type
type em_desdez from editmask within w_info_resolucionlotesdestino
end type
type em_hastaz from editmask within w_info_resolucionlotesdestino
end type
type st_15 from statictext within w_info_resolucionlotesdestino
end type
type st_16 from statictext within w_info_resolucionlotesdestino
end type
type st_17 from statictext within w_info_resolucionlotesdestino
end type
type st_18 from statictext within w_info_resolucionlotesdestino
end type
type st_19 from statictext within w_info_resolucionlotesdestino
end type
type cbx_embala from checkbox within w_info_resolucionlotesdestino
end type
type cbx_zarpe from checkbox within w_info_resolucionlotesdestino
end type
end forward

global type w_info_resolucionlotesdestino from w_para_informes
integer x = 14
integer y = 32
integer width = 2798
integer height = 2780
string icon = "AppIcon!"
gb_3 gb_3
st_14 st_14
st_44 st_44
st_1 st_1
st_2 st_2
st_3 st_3
uo_selvariedad uo_selvariedad
uo_selzonas uo_selzonas
uo_selproductor uo_selproductor
st_4 st_4
st_5 st_5
st_7 st_7
uo_selnaves uo_selnaves
uo_seltipo uo_seltipo
cbx_todocon cbx_todocon
em_contenedor em_contenedor
cbx_conscon cbx_conscon
st_8 st_8
uo_selrecibidor uo_selrecibidor
uo_selespecie uo_selespecie
st_10 st_10
rb_embalada rb_embalada
rb_embarcada rb_embarcada
uo_selembalajes uo_selembalajes
uo_selplanta uo_selplanta
st_6 st_6
st_9 st_9
st_11 st_11
ddlb_condicion ddlb_condicion
ddlb_resolucion ddlb_resolucion
st_12 st_12
st_13 st_13
cbx_todores cbx_todores
cbx_todocat cbx_todocat
em_desdee em_desdee
em_hastae em_hastae
em_desdez em_desdez
em_hastaz em_hastaz
st_15 st_15
st_16 st_16
st_17 st_17
st_18 st_18
st_19 st_19
cbx_embala cbx_embala
cbx_zarpe cbx_zarpe
end type
global w_info_resolucionlotesdestino w_info_resolucionlotesdestino

type variables
Integer	ii_Condicion
String		is_Resolucion
end variables

on w_info_resolucionlotesdestino.create
int iCurrent
call super::create
this.gb_3=create gb_3
this.st_14=create st_14
this.st_44=create st_44
this.st_1=create st_1
this.st_2=create st_2
this.st_3=create st_3
this.uo_selvariedad=create uo_selvariedad
this.uo_selzonas=create uo_selzonas
this.uo_selproductor=create uo_selproductor
this.st_4=create st_4
this.st_5=create st_5
this.st_7=create st_7
this.uo_selnaves=create uo_selnaves
this.uo_seltipo=create uo_seltipo
this.cbx_todocon=create cbx_todocon
this.em_contenedor=create em_contenedor
this.cbx_conscon=create cbx_conscon
this.st_8=create st_8
this.uo_selrecibidor=create uo_selrecibidor
this.uo_selespecie=create uo_selespecie
this.st_10=create st_10
this.rb_embalada=create rb_embalada
this.rb_embarcada=create rb_embarcada
this.uo_selembalajes=create uo_selembalajes
this.uo_selplanta=create uo_selplanta
this.st_6=create st_6
this.st_9=create st_9
this.st_11=create st_11
this.ddlb_condicion=create ddlb_condicion
this.ddlb_resolucion=create ddlb_resolucion
this.st_12=create st_12
this.st_13=create st_13
this.cbx_todores=create cbx_todores
this.cbx_todocat=create cbx_todocat
this.em_desdee=create em_desdee
this.em_hastae=create em_hastae
this.em_desdez=create em_desdez
this.em_hastaz=create em_hastaz
this.st_15=create st_15
this.st_16=create st_16
this.st_17=create st_17
this.st_18=create st_18
this.st_19=create st_19
this.cbx_embala=create cbx_embala
this.cbx_zarpe=create cbx_zarpe
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.gb_3
this.Control[iCurrent+2]=this.st_14
this.Control[iCurrent+3]=this.st_44
this.Control[iCurrent+4]=this.st_1
this.Control[iCurrent+5]=this.st_2
this.Control[iCurrent+6]=this.st_3
this.Control[iCurrent+7]=this.uo_selvariedad
this.Control[iCurrent+8]=this.uo_selzonas
this.Control[iCurrent+9]=this.uo_selproductor
this.Control[iCurrent+10]=this.st_4
this.Control[iCurrent+11]=this.st_5
this.Control[iCurrent+12]=this.st_7
this.Control[iCurrent+13]=this.uo_selnaves
this.Control[iCurrent+14]=this.uo_seltipo
this.Control[iCurrent+15]=this.cbx_todocon
this.Control[iCurrent+16]=this.em_contenedor
this.Control[iCurrent+17]=this.cbx_conscon
this.Control[iCurrent+18]=this.st_8
this.Control[iCurrent+19]=this.uo_selrecibidor
this.Control[iCurrent+20]=this.uo_selespecie
this.Control[iCurrent+21]=this.st_10
this.Control[iCurrent+22]=this.rb_embalada
this.Control[iCurrent+23]=this.rb_embarcada
this.Control[iCurrent+24]=this.uo_selembalajes
this.Control[iCurrent+25]=this.uo_selplanta
this.Control[iCurrent+26]=this.st_6
this.Control[iCurrent+27]=this.st_9
this.Control[iCurrent+28]=this.st_11
this.Control[iCurrent+29]=this.ddlb_condicion
this.Control[iCurrent+30]=this.ddlb_resolucion
this.Control[iCurrent+31]=this.st_12
this.Control[iCurrent+32]=this.st_13
this.Control[iCurrent+33]=this.cbx_todores
this.Control[iCurrent+34]=this.cbx_todocat
this.Control[iCurrent+35]=this.em_desdee
this.Control[iCurrent+36]=this.em_hastae
this.Control[iCurrent+37]=this.em_desdez
this.Control[iCurrent+38]=this.em_hastaz
this.Control[iCurrent+39]=this.st_15
this.Control[iCurrent+40]=this.st_16
this.Control[iCurrent+41]=this.st_17
this.Control[iCurrent+42]=this.st_18
this.Control[iCurrent+43]=this.st_19
this.Control[iCurrent+44]=this.cbx_embala
this.Control[iCurrent+45]=this.cbx_zarpe
end on

on w_info_resolucionlotesdestino.destroy
call super::destroy
destroy(this.gb_3)
destroy(this.st_14)
destroy(this.st_44)
destroy(this.st_1)
destroy(this.st_2)
destroy(this.st_3)
destroy(this.uo_selvariedad)
destroy(this.uo_selzonas)
destroy(this.uo_selproductor)
destroy(this.st_4)
destroy(this.st_5)
destroy(this.st_7)
destroy(this.uo_selnaves)
destroy(this.uo_seltipo)
destroy(this.cbx_todocon)
destroy(this.em_contenedor)
destroy(this.cbx_conscon)
destroy(this.st_8)
destroy(this.uo_selrecibidor)
destroy(this.uo_selespecie)
destroy(this.st_10)
destroy(this.rb_embalada)
destroy(this.rb_embarcada)
destroy(this.uo_selembalajes)
destroy(this.uo_selplanta)
destroy(this.st_6)
destroy(this.st_9)
destroy(this.st_11)
destroy(this.ddlb_condicion)
destroy(this.ddlb_resolucion)
destroy(this.st_12)
destroy(this.st_13)
destroy(this.cbx_todores)
destroy(this.cbx_todocat)
destroy(this.em_desdee)
destroy(this.em_hastae)
destroy(this.em_desdez)
destroy(this.em_hastaz)
destroy(this.st_15)
destroy(this.st_16)
destroy(this.st_17)
destroy(this.st_18)
destroy(this.st_19)
destroy(this.cbx_embala)
destroy(this.cbx_zarpe)
end on

event open;call super::open;Boolean	lb_Cerrar
String		ls_data


If IsNull(uo_SelZonas.Codigo) 		Then lb_Cerrar	=	True
If IsNull(uo_SelPlanta.Codigo) 		Then lb_Cerrar	=	True
If IsNull(uo_SelEspecie.Codigo) 	Then lb_Cerrar	=	True
If IsNull(uo_SelVariedad.Codigo)	Then lb_Cerrar	=	True
If IsNull(uo_SelProductor.Codigo) 	Then lb_Cerrar	=	True
If IsNull(uo_SelEmbalajes.Codigo) 	Then lb_Cerrar	=	True
If IsNull(uo_SelRecibidor.Codigo)	Then lb_Cerrar	=	True
If IsNull(uo_SelTipo.Codigo)			Then lb_Cerrar	=	True
If IsNull(uo_SelNaves.Codigo) 		Then lb_Cerrar	=	True

If lb_Cerrar Then
	Close(This)
Else	
	uo_SelZonas.Seleccion(True, False)
	uo_SelPlanta.Seleccion(True, True)
	uo_SelEspecie.Seleccion(True, False)
	uo_SelVariedad.Seleccion(True, False)
	uo_SelProductor.Seleccion(True, False)
	uo_SelEmbalajes.Seleccion(True, False)
	uo_SelRecibidor.Seleccion(True, False)
	uo_SelTipo.Seleccion(True, False)
	uo_SelNaves.Seleccion(True, False)
	
//	uo_SelEspecie.dw_Seleccion.Object.codigo.Dddw.PercentWidth	=	165
//	uo_SelProductor.dw_Seleccion.Object.codigo.Dddw.PercentWidth	=	195
//	uo_SelVariedad.dw_Seleccion.Object.codigo.Dddw.PercentWidth	=	165
//	uo_SelZonas.dw_Seleccion.Object.codigo.Dddw.PercentWidth		=	155
//	uo_SelTipo.dw_Seleccion.Object.codigo.Dddw.PercentWidth		=	190
//	uo_SelNaves.dw_Seleccion.Object.codigo.Dddw.PercentWidth		=	250
//	uo_SelRecibidor.dw_Seleccion.Object.codigo.Dddw.PercentWidth	=	180
//	
//	uo_SelEspecie.dw_Seleccion.Object.codigo.Dddw.HScrollBar		= 'No'
//	uo_SelProductor.dw_Seleccion.Object.codigo.Dddw.HScrollBar	= 'No'
//	uo_SelVariedad.dw_Seleccion.Object.codigo.Dddw.HScrollBar	= 'No'
//	uo_SelZonas.dw_Seleccion.Object.codigo.Dddw.HScrollBar		= 'No'
//	uo_SelTipo.dw_Seleccion.Object.codigo.Dddw.HScrollBar			= 'No'
//	uo_SelNaves.dw_Seleccion.Object.codigo.Dddw.HScrollBar		= 'No'
//	uo_SelRecibidor.dw_Seleccion.Object.codigo.Dddw.HScrollBar	= 'No'
	
	uo_SelEmbalajes.Filtra(gi_CodExport, -1)
	uo_SelTipo.Codigo = 'M'
	uo_SelTipo.dw_Seleccion.Object.Codigo[1] = 'M'
	uo_SelTipo.cbx_Todos.Checked = False
	uo_SelNaves.Filtra('M')
	
	em_DesdeE.Text	=	'01/01/2000'
	em_HastaE.Text	=	String(Today(), 'dd/mm/yyyy')
	em_DesdeZ.Text	=	'01/01/2000'
	em_HastaZ.Text	=	String(Today(), 'dd/mm/yyyy')
	is_Resolucion		=	'*'
	ii_Condicion			=	-1
End If
end event

type pb_excel from w_para_informes`pb_excel within w_info_resolucionlotesdestino
end type

type st_computador from w_para_informes`st_computador within w_info_resolucionlotesdestino
end type

type st_usuario from w_para_informes`st_usuario within w_info_resolucionlotesdestino
end type

type st_temporada from w_para_informes`st_temporada within w_info_resolucionlotesdestino
end type

type p_logo from w_para_informes`p_logo within w_info_resolucionlotesdestino
integer x = 5
end type

type st_titulo from w_para_informes`st_titulo within w_info_resolucionlotesdestino
integer width = 2021
string text = "Resolución con Lotes Destino"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_resolucionlotesdestino
string tag = "Imprimir Reporte"
integer x = 2418
integer y = 740
integer taborder = 100
end type

event pb_acepta::clicked;Integer	li_fila
String		ls_Contenedor

istr_info.titulo	= 'RESUMEN PLANILLA RECLAMOS DE ' + Upper(uo_SelEspecie.Nombre)
OpenWithParm(vinf,istr_info)

If cbx_ConsCon.Checked Then
	ls_Contenedor	= '**'
ElseIf cbx_TodoCon.Checked Then
	ls_Contenedor	= '*'
Else
	ls_Contenedor	= em_Contenedor.Text
End If

If rb_Embalada.Checked Then
	vinf.dw_1.DataObject = "dw_info_lotesdestino_frutaembalada"	
ElseIf rb_embarcada.Checked Then
	vinf.dw_1.DataObject = "dw_info_lotesdestino_frutaembarcada"	
End If

vinf.dw_1.SetTransObject(sqlca)

li_fila = vinf.dw_1.Retrieve(uo_SelZonas.Codigo, uo_SelPlanta.Codigo, uo_SelEspecie.Codigo, uo_SelVariedad.Codigo, uo_SelProductor.Codigo, &
								uo_SelEmbalajes.Codigo, is_Resolucion, ii_Condicion, uo_SelRecibidor.Codigo, uo_SelTipo.Codigo, uo_SelNaves.Codigo,&
								ls_Contenedor, Date(em_DesdeE.Text), Date(em_HastaE.Text), Date(em_DesdeZ.Text), Date(em_HastaZ.Text))
						  
If li_fila = -1 Then
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ElseIf li_fila = 0 Then
	MessageBox( "No Existe información", "No existe información para este informe.", StopSign!, Ok!)
Else
	F_Membrete(vinf.dw_1)
	vinf.dw_1.ModIfy('DataWindow.Zoom = 52')
	If gs_Ambiente <> 'Windows' Then F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
End If
end event

type pb_salir from w_para_informes`pb_salir within w_info_resolucionlotesdestino
string tag = "Salir [Cerrar Ventana Activa]"
integer x = 2423
integer y = 1028
integer taborder = 110
string powertiptext = "Salir [Cerrar Ventana Activa]"
end type

type gb_3 from groupbox within w_info_resolucionlotesdestino
integer x = 311
integer y = 2368
integer width = 1874
integer height = 216
integer taborder = 120
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = " Tipo de Informe "
end type

type st_14 from statictext within w_info_resolucionlotesdestino
integer x = 297
integer y = 504
integer width = 370
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
string text = "Zona"
boolean focusrectangle = false
end type

type st_44 from statictext within w_info_resolucionlotesdestino
integer x = 251
integer y = 416
integer width = 2016
integer height = 1564
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

type st_1 from statictext within w_info_resolucionlotesdestino
integer x = 297
integer y = 748
integer width = 370
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
string text = "Especie"
boolean focusrectangle = false
end type

type st_2 from statictext within w_info_resolucionlotesdestino
integer x = 297
integer y = 872
integer width = 370
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
string text = "Variedad"
boolean focusrectangle = false
end type

type st_3 from statictext within w_info_resolucionlotesdestino
integer x = 297
integer y = 992
integer width = 370
integer height = 84
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
string text = "Productor"
boolean focusrectangle = false
end type

type uo_selvariedad from uo_seleccion_variedad_mod within w_info_resolucionlotesdestino
integer x = 923
integer y = 848
integer width = 1019
integer taborder = 20
boolean bringtotop = true
end type

on uo_selvariedad.destroy
call uo_seleccion_variedad_mod::destroy
end on

type uo_selzonas from uo_seleccion_zonas_mod within w_info_resolucionlotesdestino
integer x = 923
integer y = 480
integer width = 1019
integer taborder = 30
boolean bringtotop = true
end type

on uo_selzonas.destroy
call uo_seleccion_zonas_mod::destroy
end on

event ue_cambio;call super::ue_cambio;If IsNull(This.Codigo) Then Return

Choose Case This.Codigo
	Case -1, -9
		uo_SelProductor.Filtra(-1)
		
	Case Else
		uo_SelProductor.Filtra(This.Codigo)
		
End Choose
end event

type uo_selproductor from uo_seleccion_productor_zonas_mod within w_info_resolucionlotesdestino
integer x = 923
integer y = 968
integer width = 1019
integer taborder = 40
boolean bringtotop = true
end type

on uo_selproductor.destroy
call uo_seleccion_productor_zonas_mod::destroy
end on

type st_4 from statictext within w_info_resolucionlotesdestino
integer x = 2007
integer y = 436
integer width = 210
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
string text = "Consol"
boolean focusrectangle = false
end type

type st_5 from statictext within w_info_resolucionlotesdestino
integer x = 297
integer y = 1508
integer width = 471
integer height = 84
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
string text = "Recibidor"
boolean focusrectangle = false
end type

type st_7 from statictext within w_info_resolucionlotesdestino
integer x = 297
integer y = 1748
integer width = 471
integer height = 84
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
string text = "Nave"
boolean focusrectangle = false
end type

type uo_selnaves from uo_seleccion_naves_mod within w_info_resolucionlotesdestino
integer x = 910
integer y = 1716
integer width = 1019
integer taborder = 70
boolean bringtotop = true
end type

on uo_selnaves.destroy
call uo_seleccion_naves_mod::destroy
end on

type uo_seltipo from uo_seleccion_tipotransporte_mod within w_info_resolucionlotesdestino
integer x = 914
integer y = 1608
integer width = 1019
integer taborder = 60
boolean bringtotop = true
end type

on uo_seltipo.destroy
call uo_seleccion_tipotransporte_mod::destroy
end on

event ue_cambio;call super::ue_cambio;If IsNull(This.Codigo) Then Return

Choose Case This.Codigo
	Case '*', '**'
		
	Case Else
		uo_SelNaves.Filtra(This.Codigo)
		
End Choose
end event

type cbx_todocon from checkbox within w_info_resolucionlotesdestino
integer x = 1842
integer y = 1852
integer width = 82
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long backcolor = 553648127
boolean checked = true
end type

event clicked;If This.Checked Then
	em_Contenedor.Text		=	''
	em_Contenedor.Enabled	=	False
Else
	em_Contenedor.Text		=	''
	em_Contenedor.Enabled	=	True
	em_Contenedor.SetFocus()
End If
end event

type em_contenedor from editmask within w_info_resolucionlotesdestino
integer x = 923
integer y = 1844
integer width = 869
integer height = 92
integer taborder = 90
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long backcolor = 16777215
boolean enabled = false
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = stringmask!
string minmax = "~~20"
end type

type cbx_conscon from checkbox within w_info_resolucionlotesdestino
integer x = 2048
integer y = 1852
integer width = 82
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long backcolor = 553648127
end type

event clicked;If This.Checked Then
	em_Contenedor.Text		=	''
	em_Contenedor.Enabled	=	False
	cbx_TodoCon.Enabled		=	False
	cbx_TodoCon.Checked	=	True
Else
	cbx_TodoCon.Enabled		=	True
	cbx_TodoCon.Checked	=	True
	em_Contenedor.SetFocus()
End If
end event

type st_8 from statictext within w_info_resolucionlotesdestino
integer x = 297
integer y = 1628
integer width = 471
integer height = 84
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
string text = "Tipo Transporte"
boolean focusrectangle = false
end type

type uo_selrecibidor from uo_seleccion_recibidor_mod within w_info_resolucionlotesdestino
integer x = 914
integer y = 1480
integer width = 1019
integer taborder = 50
boolean bringtotop = true
end type

on uo_selrecibidor.destroy
call uo_seleccion_recibidor_mod::destroy
end on

type uo_selespecie from uo_seleccion_especie_mod within w_info_resolucionlotesdestino
integer x = 923
integer y = 724
integer width = 1019
integer taborder = 110
boolean bringtotop = true
end type

on uo_selespecie.destroy
call uo_seleccion_especie_mod::destroy
end on

event ue_cambio;call super::ue_cambio;If IsNull(This.Codigo) Then Return

Choose Case This.Codigo
	Case -1, -9
		
	Case Else
		uo_SelVariedad.Filtra(This.Codigo)
		uo_SelEmbalajes.Filtra(gi_CodExport, This.Codigo)
		
End Choose
end event

type st_10 from statictext within w_info_resolucionlotesdestino
integer x = 297
integer y = 1852
integer width = 471
integer height = 84
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
string text = "Contenedor"
boolean focusrectangle = false
end type

type rb_embalada from radiobutton within w_info_resolucionlotesdestino
integer x = 530
integer y = 2460
integer width = 585
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
string text = "Fruta Embalada"
end type

type rb_embarcada from radiobutton within w_info_resolucionlotesdestino
integer x = 1362
integer y = 2460
integer width = 599
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
string text = "Fruta Embarcada"
boolean checked = true
end type

type uo_selembalajes from uo_seleccion_embalajesprod_mod within w_info_resolucionlotesdestino
event destroy ( )
integer x = 923
integer y = 1088
integer width = 1019
integer taborder = 50
boolean bringtotop = true
end type

on uo_selembalajes.destroy
call uo_seleccion_embalajesprod_mod::destroy
end on

type uo_selplanta from uo_seleccion_frigorifico_mod within w_info_resolucionlotesdestino
integer x = 923
integer y = 600
integer taborder = 120
boolean bringtotop = true
end type

on uo_selplanta.destroy
call uo_seleccion_frigorifico_mod::destroy
end on

type st_6 from statictext within w_info_resolucionlotesdestino
integer x = 297
integer y = 624
integer width = 370
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
string text = "Frigorifico"
boolean focusrectangle = false
end type

type st_9 from statictext within w_info_resolucionlotesdestino
integer x = 297
integer y = 1372
integer width = 608
integer height = 84
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
string text = "Categoria Condición"
boolean focusrectangle = false
end type

type st_11 from statictext within w_info_resolucionlotesdestino
integer x = 1787
integer y = 436
integer width = 192
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
string text = "Todos"
boolean focusrectangle = false
end type

type ddlb_condicion from dropdownlistbox within w_info_resolucionlotesdestino
integer x = 928
integer y = 1356
integer width = 873
integer height = 400
integer taborder = 50
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
boolean enabled = false
boolean autohscroll = true
boolean sorted = false
boolean vscrollbar = true
string item[] = {"1","2","3","4"," "}
borderstyle borderstyle = stylelowered!
end type

event selectionchanged;Choose Case Index
	Case 1
		ii_Condicion = 1
		
	Case 2
		ii_Condicion = 2
		
	Case 3
		ii_Condicion = 3
		
	Case 4
		ii_Condicion = 4
		
	Case 5
		ii_Condicion = 0
		
End Choose
end event

type ddlb_resolucion from dropdownlistbox within w_info_resolucionlotesdestino
integer x = 928
integer y = 1232
integer width = 873
integer height = 400
integer taborder = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
boolean enabled = false
boolean allowedit = true
boolean autohscroll = true
boolean sorted = false
boolean vscrollbar = true
string item[] = {"Aprobado","Comercial","Objetado"}
borderstyle borderstyle = stylelowered!
end type

event selectionchanged;Choose Case Index
	Case 1
		is_Resolucion = 'A'
		
	Case 2 
		is_Resolucion = 'C'
		
	Case 3
		is_Resolucion = 'O'
		
End Choose
end event

type st_12 from statictext within w_info_resolucionlotesdestino
integer x = 293
integer y = 1240
integer width = 576
integer height = 84
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
string text = "Resolución Calidad"
boolean focusrectangle = false
end type

type st_13 from statictext within w_info_resolucionlotesdestino
integer x = 297
integer y = 1112
integer width = 370
integer height = 84
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
string text = "Embalaje"
boolean focusrectangle = false
end type

type cbx_todores from checkbox within w_info_resolucionlotesdestino
integer x = 1842
integer y = 1240
integer width = 82
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long backcolor = 553648127
boolean checked = true
end type

event clicked;If This.Checked Then
	ddlb_Resolucion.Enabled	=	False
	is_Resolucion = '*'
Else
	ddlb_Resolucion.Enabled	=	True
	ddlb_Resolucion.SetFocus()
End If
end event

type cbx_todocat from checkbox within w_info_resolucionlotesdestino
integer x = 1842
integer y = 1372
integer width = 82
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long backcolor = 553648127
boolean checked = true
end type

event clicked;If This.Checked Then
	ddlb_Condicion.Enabled	=	False
	ii_Condicion					=	-1
Else
	ddlb_Condicion.Enabled	=	True
	ddlb_Condicion.SetFocus()
End If
end event

type em_desdee from editmask within w_info_resolucionlotesdestino
integer x = 887
integer y = 2108
integer width = 475
integer height = 88
integer taborder = 100
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
boolean enabled = false
string text = "none"
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
boolean dropdowncalendar = true
boolean dropdownright = true
end type

type em_hastae from editmask within w_info_resolucionlotesdestino
integer x = 1376
integer y = 2108
integer width = 475
integer height = 88
integer taborder = 100
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
boolean enabled = false
string text = "none"
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
boolean dropdowncalendar = true
boolean dropdownright = true
end type

type em_desdez from editmask within w_info_resolucionlotesdestino
integer x = 887
integer y = 2236
integer width = 475
integer height = 88
integer taborder = 110
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
boolean enabled = false
string text = "none"
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
boolean dropdowncalendar = true
boolean dropdownright = true
end type

type em_hastaz from editmask within w_info_resolucionlotesdestino
integer x = 1376
integer y = 2236
integer width = 475
integer height = 88
integer taborder = 110
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
boolean enabled = false
string text = "none"
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
boolean dropdowncalendar = true
boolean dropdownright = true
end type

type st_15 from statictext within w_info_resolucionlotesdestino
integer x = 923
integer y = 2024
integer width = 402
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
alignment alignment = center!
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_16 from statictext within w_info_resolucionlotesdestino
integer x = 1376
integer y = 2020
integer width = 402
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
alignment alignment = center!
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_17 from statictext within w_info_resolucionlotesdestino
integer x = 297
integer y = 2120
integer width = 485
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
string text = "Fecha Embalaje"
boolean focusrectangle = false
end type

type st_18 from statictext within w_info_resolucionlotesdestino
integer x = 297
integer y = 2248
integer width = 402
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
string text = "Fecha Zarpe"
boolean focusrectangle = false
end type

type st_19 from statictext within w_info_resolucionlotesdestino
integer x = 251
integer y = 1980
integer width = 2016
integer height = 656
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16711680
long backcolor = 16711680
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type cbx_embala from checkbox within w_info_resolucionlotesdestino
integer x = 1883
integer y = 2112
integer width = 82
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long backcolor = 553648127
boolean checked = true
end type

event clicked;If This.Checked Then
	em_DesdeE.Enabled	=	False
	em_HastaE.Enabled	=	False
	em_DesdeE.Text		=	'01/01/2000'
	em_HastaE.Text		=	String(Today(), 'dd/mm/yyyy')
Else
	em_DesdeE.Enabled	=	True
	em_HastaE.Enabled	=	True
	em_DesdeE.Text		=	'01/' + String(Today(), 'mm/yyyy')
	em_HastaE.Text		=	String(Today(), 'dd/mm/yyyy')
End If
end event

type cbx_zarpe from checkbox within w_info_resolucionlotesdestino
integer x = 1883
integer y = 2240
integer width = 82
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long backcolor = 553648127
boolean checked = true
end type

event clicked;If This.Checked Then
	em_DesdeZ.Enabled	=	False
	em_HastaZ.Enabled	=	False
	em_DesdeZ.Text		=	'01/01/2000'
	em_HastaZ.Text		=	String(Today(), 'dd/mm/yyyy')
Else
	em_DesdeZ.Enabled	=	True
	em_HastaZ.Enabled	=	True
	em_DesdeZ.Text		=	'01/' + String(Today(), 'mm/yyyy')
	em_HastaZ.Text		=	String(Today(), 'dd/mm/yyyy')
End If
end event

