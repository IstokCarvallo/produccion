$PBExportHeader$w_info_planillainspeccion_destino.srw
$PBExportComments$Ventana de Informe de Resumen Objeciones
forward
global type w_info_planillainspeccion_destino from w_para_informes
end type
type em_fechadesde from editmask within w_info_planillainspeccion_destino
end type
type st_12 from statictext within w_info_planillainspeccion_destino
end type
type st_13 from statictext within w_info_planillainspeccion_destino
end type
type em_fechahasta from editmask within w_info_planillainspeccion_destino
end type
type st_zona from statictext within w_info_planillainspeccion_destino
end type
type st_33 from statictext within w_info_planillainspeccion_destino
end type
type cbx_todosfecha from checkbox within w_info_planillainspeccion_destino
end type
type st_9 from statictext within w_info_planillainspeccion_destino
end type
type st_10 from statictext within w_info_planillainspeccion_destino
end type
type cbx_nave from checkbox within w_info_planillainspeccion_destino
end type
type st_11 from statictext within w_info_planillainspeccion_destino
end type
type dw_nave from datawindow within w_info_planillainspeccion_destino
end type
type st_14 from statictext within w_info_planillainspeccion_destino
end type
type st_1 from statictext within w_info_planillainspeccion_destino
end type
type st_3 from statictext within w_info_planillainspeccion_destino
end type
type st_5 from statictext within w_info_planillainspeccion_destino
end type
type ddlb_mercado from dropdownlistbox within w_info_planillainspeccion_destino
end type
type cbx_mercado from checkbox within w_info_planillainspeccion_destino
end type
type dw_planilla from datawindow within w_info_planillainspeccion_destino
end type
type cbx_planilla from checkbox within w_info_planillainspeccion_destino
end type
type gb_3 from groupbox within w_info_planillainspeccion_destino
end type
type st_7 from statictext within w_info_planillainspeccion_destino
end type
type st_2 from statictext within w_info_planillainspeccion_destino
end type
type uo_muestrarecibidor from uo_seleccion_recibidor_planilla_mod within w_info_planillainspeccion_destino
end type
type uo_tipotrans from uo_seleccion_tipotransporte_mod within w_info_planillainspeccion_destino
end type
type uo_muestrazona from uo_seleccion_zonas_mod within w_info_planillainspeccion_destino
end type
type uo_muestraespecies from uo_seleccion_especie_mod within w_info_planillainspeccion_destino
end type
type uo_muestraproductor from uo_seleccion_productor_zonas_mod within w_info_planillainspeccion_destino
end type
type uo_muestravariedad from uo_seleccion_variedad_mod within w_info_planillainspeccion_destino
end type
type st_15 from statictext within w_info_planillainspeccion_destino
end type
type gb_4 from groupbox within w_info_planillainspeccion_destino
end type
type st_8 from statictext within w_info_planillainspeccion_destino
end type
type st_6 from statictext within w_info_planillainspeccion_destino
end type
type st_16 from statictext within w_info_planillainspeccion_destino
end type
type uo_muestraembalaje from uo_seleccion_embalaje_mod within w_info_planillainspeccion_destino
end type
type uo_muestracalibre from uo_seleccion_calibre_mod within w_info_planillainspeccion_destino
end type
type gb_5 from groupbox within w_info_planillainspeccion_destino
end type
type st_4 from statictext within w_info_planillainspeccion_destino
end type
type cbx_todosrec from checkbox within w_info_planillainspeccion_destino
end type
type rb_si from radiobutton within w_info_planillainspeccion_destino
end type
type rb_no from radiobutton within w_info_planillainspeccion_destino
end type
end forward

global type w_info_planillainspeccion_destino from w_para_informes
integer x = 14
integer y = 32
integer width = 3616
integer height = 2020
string title = "Consulta Planilla Inspeccion en Destino"
boolean minbox = false
boolean maxbox = false
boolean resizable = false
windowtype windowtype = response!
string icon = "F:\Desarrollo\Producción\FrutaProcesada\Producc.ico"
em_fechadesde em_fechadesde
st_12 st_12
st_13 st_13
em_fechahasta em_fechahasta
st_zona st_zona
st_33 st_33
cbx_todosfecha cbx_todosfecha
st_9 st_9
st_10 st_10
cbx_nave cbx_nave
st_11 st_11
dw_nave dw_nave
st_14 st_14
st_1 st_1
st_3 st_3
st_5 st_5
ddlb_mercado ddlb_mercado
cbx_mercado cbx_mercado
dw_planilla dw_planilla
cbx_planilla cbx_planilla
gb_3 gb_3
st_7 st_7
st_2 st_2
uo_muestrarecibidor uo_muestrarecibidor
uo_tipotrans uo_tipotrans
uo_muestrazona uo_muestrazona
uo_muestraespecies uo_muestraespecies
uo_muestraproductor uo_muestraproductor
uo_muestravariedad uo_muestravariedad
st_15 st_15
gb_4 gb_4
st_8 st_8
st_6 st_6
st_16 st_16
uo_muestraembalaje uo_muestraembalaje
uo_muestracalibre uo_muestracalibre
gb_5 gb_5
st_4 st_4
cbx_todosrec cbx_todosrec
rb_si rb_si
rb_no rb_no
end type
global w_info_planillainspeccion_destino w_info_planillainspeccion_destino

type variables
str_busqueda istr_busq
str_mant istr_mant
Integer	ii_tipo, ii_reclamo
String	is_report, is_nula

DataWindowChild		idwc_nave, idwc_planilla
uo_naves             iuo_naves
uo_nroplanilla       iuo_nroplanilla




end variables

on w_info_planillainspeccion_destino.create
int iCurrent
call super::create
this.em_fechadesde=create em_fechadesde
this.st_12=create st_12
this.st_13=create st_13
this.em_fechahasta=create em_fechahasta
this.st_zona=create st_zona
this.st_33=create st_33
this.cbx_todosfecha=create cbx_todosfecha
this.st_9=create st_9
this.st_10=create st_10
this.cbx_nave=create cbx_nave
this.st_11=create st_11
this.dw_nave=create dw_nave
this.st_14=create st_14
this.st_1=create st_1
this.st_3=create st_3
this.st_5=create st_5
this.ddlb_mercado=create ddlb_mercado
this.cbx_mercado=create cbx_mercado
this.dw_planilla=create dw_planilla
this.cbx_planilla=create cbx_planilla
this.gb_3=create gb_3
this.st_7=create st_7
this.st_2=create st_2
this.uo_muestrarecibidor=create uo_muestrarecibidor
this.uo_tipotrans=create uo_tipotrans
this.uo_muestrazona=create uo_muestrazona
this.uo_muestraespecies=create uo_muestraespecies
this.uo_muestraproductor=create uo_muestraproductor
this.uo_muestravariedad=create uo_muestravariedad
this.st_15=create st_15
this.gb_4=create gb_4
this.st_8=create st_8
this.st_6=create st_6
this.st_16=create st_16
this.uo_muestraembalaje=create uo_muestraembalaje
this.uo_muestracalibre=create uo_muestracalibre
this.gb_5=create gb_5
this.st_4=create st_4
this.cbx_todosrec=create cbx_todosrec
this.rb_si=create rb_si
this.rb_no=create rb_no
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.em_fechadesde
this.Control[iCurrent+2]=this.st_12
this.Control[iCurrent+3]=this.st_13
this.Control[iCurrent+4]=this.em_fechahasta
this.Control[iCurrent+5]=this.st_zona
this.Control[iCurrent+6]=this.st_33
this.Control[iCurrent+7]=this.cbx_todosfecha
this.Control[iCurrent+8]=this.st_9
this.Control[iCurrent+9]=this.st_10
this.Control[iCurrent+10]=this.cbx_nave
this.Control[iCurrent+11]=this.st_11
this.Control[iCurrent+12]=this.dw_nave
this.Control[iCurrent+13]=this.st_14
this.Control[iCurrent+14]=this.st_1
this.Control[iCurrent+15]=this.st_3
this.Control[iCurrent+16]=this.st_5
this.Control[iCurrent+17]=this.ddlb_mercado
this.Control[iCurrent+18]=this.cbx_mercado
this.Control[iCurrent+19]=this.dw_planilla
this.Control[iCurrent+20]=this.cbx_planilla
this.Control[iCurrent+21]=this.gb_3
this.Control[iCurrent+22]=this.st_7
this.Control[iCurrent+23]=this.st_2
this.Control[iCurrent+24]=this.uo_muestrarecibidor
this.Control[iCurrent+25]=this.uo_tipotrans
this.Control[iCurrent+26]=this.uo_muestrazona
this.Control[iCurrent+27]=this.uo_muestraespecies
this.Control[iCurrent+28]=this.uo_muestraproductor
this.Control[iCurrent+29]=this.uo_muestravariedad
this.Control[iCurrent+30]=this.st_15
this.Control[iCurrent+31]=this.gb_4
this.Control[iCurrent+32]=this.st_8
this.Control[iCurrent+33]=this.st_6
this.Control[iCurrent+34]=this.st_16
this.Control[iCurrent+35]=this.uo_muestraembalaje
this.Control[iCurrent+36]=this.uo_muestracalibre
this.Control[iCurrent+37]=this.gb_5
this.Control[iCurrent+38]=this.st_4
this.Control[iCurrent+39]=this.cbx_todosrec
this.Control[iCurrent+40]=this.rb_si
this.Control[iCurrent+41]=this.rb_no
end on

on w_info_planillainspeccion_destino.destroy
call super::destroy
destroy(this.em_fechadesde)
destroy(this.st_12)
destroy(this.st_13)
destroy(this.em_fechahasta)
destroy(this.st_zona)
destroy(this.st_33)
destroy(this.cbx_todosfecha)
destroy(this.st_9)
destroy(this.st_10)
destroy(this.cbx_nave)
destroy(this.st_11)
destroy(this.dw_nave)
destroy(this.st_14)
destroy(this.st_1)
destroy(this.st_3)
destroy(this.st_5)
destroy(this.ddlb_mercado)
destroy(this.cbx_mercado)
destroy(this.dw_planilla)
destroy(this.cbx_planilla)
destroy(this.gb_3)
destroy(this.st_7)
destroy(this.st_2)
destroy(this.uo_muestrarecibidor)
destroy(this.uo_tipotrans)
destroy(this.uo_muestrazona)
destroy(this.uo_muestraespecies)
destroy(this.uo_muestraproductor)
destroy(this.uo_muestravariedad)
destroy(this.st_15)
destroy(this.gb_4)
destroy(this.st_8)
destroy(this.st_6)
destroy(this.st_16)
destroy(this.uo_muestraembalaje)
destroy(this.uo_muestracalibre)
destroy(this.gb_5)
destroy(this.st_4)
destroy(this.cbx_todosrec)
destroy(this.rb_si)
destroy(this.rb_no)
end on

event open;call super::open;
x = 0
y = 0
cbx_mercado.SetFocus()
This.Icon	=	Gstr_apl.Icono

iuo_naves             =  Create uo_naves
iuo_nroplanilla       =  Create uo_nroplanilla

Boolean	lb_Cerrar

IF IsNull(uo_tipotrans.Codigo) THEN lb_Cerrar	=	True
IF IsNull(uo_muestrarecibidor.Codigo) THEN lb_Cerrar	=	True
IF IsNull(uo_muestraespecies.Codigo) THEN lb_Cerrar	=	True
IF IsNull(uo_muestravariedad.Codigo) THEN lb_Cerrar	=	True
IF IsNull(uo_muestrazona.Codigo) THEN lb_Cerrar	=	True
IF IsNull(uo_muestraproductor.Codigo) THEN lb_Cerrar	=	True
IF IsNull(uo_muestraembalaje.Codigo) THEN lb_Cerrar	=	True
IF IsNull(uo_muestracalibre.Codigo) THEN lb_Cerrar	=	True

IF lb_Cerrar THEN
	Close(This)
ELSE
	uo_tipotrans.Seleccion(True, True)
	uo_muestrarecibidor.Seleccion(True, True)
	uo_muestraespecies.Seleccion(True, True)
	uo_muestravariedad.Seleccion(True, True)
	uo_muestrazona.Seleccion(True, True)
	uo_muestraproductor.Seleccion(True, True)
	uo_muestraembalaje.Seleccion(True, True)
	uo_muestracalibre.Seleccion(True, True)

	//Nave							
	dw_nave.GetChild("nave_codigo", idwc_nave)
	idwc_nave.SetTransObject(sqlca)
	idwc_nave.Retrieve(0, '*')
	dw_nave.InsertRow(0)
	idwc_nave.SetSort("nave_nombre A")
	idwc_nave.Sort()
	//dw_nave.enabled = false
	
	
	//nro planilla							
	dw_planilla.GetChild("num_planil", idwc_planilla)
	idwc_planilla.SetTransObject(sqlca)
	idwc_planilla.Retrieve(gi_CodExport)
	dw_planilla.InsertRow(0)
	//dw_planilla.enabled = false
	
	//TipoTransporte
	uo_tipotrans.cbx_consolida.checked  = FALSE
	uo_tipotrans.cbx_consolida.visible  = FALSE
	uo_tipotrans.cbx_todos.checked      = FALSE
	uo_tipotrans.cbx_todos.visible      = FALSE
	uo_tipotrans.dw_Seleccion.Enabled	= TRUE
	uo_tipotrans.dw_seleccion.object.codigo[1] ='M'
	
	//Recibidor
	uo_muestrarecibidor.cbx_consolida.checked = FALSE
	uo_muestrarecibidor.cbx_consolida.visible = FALSE
	uo_muestrarecibidor.filtra(0)
	
	//Especie
	uo_muestraespecies.cbx_todos.checked     = FALSE
	uo_muestraespecies.cbx_todos.visible     = FALSE
	uo_muestraespecies.cbx_consolida.checked = FALSE
	uo_muestraespecies.cbx_consolida.visible = FALSE
	uo_muestraespecies.dw_seleccion.object.codigo[1] = 11
	uo_muestraespecies.dw_seleccion.enabled  = TRUE
	 
	//Variedad
	uo_muestravariedad.cbx_consolida.checked = FALSE
	uo_muestravariedad.cbx_consolida.visible = FALSE
	uo_muestravariedad.filtra(11)
	
	//Zona
	uo_muestrazona.cbx_consolida.checked = FALSE
	uo_muestrazona.cbx_consolida.visible = FALSE
	uo_muestrazona.dw_seleccion.Object.Codigo[1] = Gi_CodZona
	
	//Productor
	uo_muestraproductor.cbx_consolida.checked = FALSE
	uo_muestraproductor.cbx_consolida.visible = FALSE
	uo_muestraproductor.filtra(0)
	
	//Embalaje
	uo_muestraembalaje.cbx_consolida.checked = FALSE
	uo_muestraembalaje.cbx_consolida.visible = FALSE
	uo_muestraembalaje.filtra('U')
	
	//Calibre
	uo_muestracalibre.cbx_consolida.checked = FALSE
	uo_muestracalibre.cbx_consolida.visible = FALSE
	uo_muestracalibre.filtra(81,11,0)
	
	ii_Reclamo = -1
	
	istr_mant.argumento[1] = ''
	em_fechadesde.text	  = String(Today())
	em_fechahasta.text	  = String(Today())
End If
end event

type st_computador from w_para_informes`st_computador within w_info_planillainspeccion_destino
integer x = 2734
end type

type st_usuario from w_para_informes`st_usuario within w_info_planillainspeccion_destino
integer x = 2734
end type

type st_temporada from w_para_informes`st_temporada within w_info_planillainspeccion_destino
integer x = 2734
end type

type p_logo from w_para_informes`p_logo within w_info_planillainspeccion_destino
end type

type st_titulo from w_para_informes`st_titulo within w_info_planillainspeccion_destino
integer width = 2971
string text = "Consulta Planilla Inspección en Destino"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_planillainspeccion_destino
string tag = "Imprimir Reporte"
integer x = 3337
integer y = 476
integer taborder = 170
string powertiptext = "Imprimir Reporte"
end type

event pb_acepta::clicked;Integer	li_fila, li_mercado, li_nave, li_consfechaemb 
Date		ld_FechaEmbaini, ld_FechaEmbafin
Long     	ll_planilla

//Mercado
If cbx_mercado.Checked Then
   li_mercado	= -1
Else
   li_mercado	= Integer(istr_mant.argumento[1])
	If IsNull(li_mercado) OR li_mercado=0 Then
	   MessageBox("Atención","Debe Seleccionar un Mercado Previamente",Exclamation!)
	   Return
	End If
End If

//Nave
If cbx_nave.Checked Then
   li_nave	= -1
Else
   li_nave	= dw_nave.Object.nave_codigo[1]
	If IsNull(li_nave)Then
	   MessageBox("Atención","Debe Seleccionar una Nave Previamente",Exclamation!)
	   Return
	End If
End If

//NroPlanilla
If cbx_planilla.Checked Then
   ll_planilla	= -1
Else
   ll_planilla	= dw_planilla.Object.num_planil[1]
	If IsNull(ll_planilla)Then
	   MessageBox("Atención","Debe Seleccionar un Nro de Planilla Previamente",Exclamation!)
	   Return
	End If
End If

//Reclamo
	If IsNull(ii_reclamo) OR ii_reclamo = 0 Then
	   MessageBox("Atención","Debe Seleccionar una Opción de reclamo Previamente",Exclamation!)
	   Return
	End If

//Fecha Embalaje
If cbx_todosfecha.Checked Then
	ld_FechaEmbaini =	Date(01/01/2000)
	ld_FechaEmbafin =	Today()
Else
	ld_FechaEmbaini = Date(em_fechadesde.Text)
	ld_FechaEmbafin = Date(em_fechahasta.Text)
End If

istr_info.titulo	= 'PLANILLA DE INSPECCION UVA DE MESA EN DESTINO'
OpenWithParm(vinf,istr_info)  
vinf.dw_1.DataObject = "dw_info_planillainspeccion_destino_detal"
vinf.dw_1.SetTransObject(sqlca)

li_fila = vinf.dw_1.Retrieve(Gi_CodExport,li_mercado,uo_tipotrans.Codigo,li_nave,uo_MuestraRecibidor.Codigo,uo_MuestraZona.Codigo,&
                             uo_MuestraProductor.Codigo,uo_MuestraEspecies.Codigo,uo_MuestraVariedad.Codigo,ld_FechaEmbaini,ld_FechaEmbafin,&
						ll_planilla,uo_MuestraEmbalaje.Codigo,uo_MuestraCalibre.Codigo,ii_reclamo)

If li_fila = -1 Then
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ElseIf li_fila = 0 Then
	MessageBox( "No Existe información", "No existe información para este informe.", StopSign!, Ok!)
Else
	F_Membrete(vinf.dw_1)
	vinf.dw_1.ModIfy("desde.text = '" + em_fechadesde.text + "'")
	vinf.dw_1.ModIfy("hasta.text = '" + em_fechahasta.text + "'")	
	vinf.dw_1.Object.DataWindow.Zoom = 65

	If gs_Ambiente <> 'Windows' Then F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
End If
end event

type pb_salir from w_para_informes`pb_salir within w_info_planillainspeccion_destino
string tag = "Salir [Cerrar Ventana Activa]"
integer x = 3337
integer y = 760
integer taborder = 180
string powertiptext = "Salir [Cerrar Ventana Activa]"
end type

type em_fechadesde from editmask within w_info_planillainspeccion_destino
integer x = 1888
integer y = 1548
integer width = 411
integer height = 92
integer taborder = 140
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 16777215
boolean enabled = false
string text = "none"
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
boolean dropdowncalendar = true
end type

type st_12 from statictext within w_info_planillainspeccion_destino
integer x = 1893
integer y = 1488
integer width = 201
integer height = 60
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 33543637
boolean enabled = false
string text = "Desde"
boolean focusrectangle = false
end type

type st_13 from statictext within w_info_planillainspeccion_destino
integer x = 2537
integer y = 1492
integer width = 178
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 33543637
boolean enabled = false
string text = "Hasta"
boolean focusrectangle = false
end type

type em_fechahasta from editmask within w_info_planillainspeccion_destino
integer x = 2533
integer y = 1548
integer width = 411
integer height = 92
integer taborder = 150
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 16777215
boolean enabled = false
string text = "none"
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
boolean dropdowncalendar = true
end type

type st_zona from statictext within w_info_planillainspeccion_destino
integer x = 297
integer y = 1076
integer width = 389
integer height = 88
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
boolean enabled = false
string text = "Zona Origen"
boolean focusrectangle = false
end type

type st_33 from statictext within w_info_planillainspeccion_destino
integer x = 297
integer y = 1228
integer width = 389
integer height = 84
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
boolean enabled = false
string text = "Productor"
boolean focusrectangle = false
end type

type cbx_todosfecha from checkbox within w_info_planillainspeccion_destino
integer x = 2999
integer y = 1560
integer width = 110
integer height = 72
integer taborder = 130
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 33543637
string text = " "
boolean checked = true
end type

event clicked;IF This.Checked THEN
	em_fechadesde.Enabled	=	FALSE
	em_fechahasta.Enabled	=	FALSE
   istr_Mant.Argumento[12]	=	'01/01/1900'
	istr_Mant.Argumento[13]	=	String(Today())
ELSE
	em_fechadesde.Enabled	=	TRUE
	em_fechahasta.Enabled	=	TRUE
	em_fechadesde.SetFocus()
END IF
RETURN 0



end event

type st_9 from statictext within w_info_planillainspeccion_destino
integer x = 283
integer y = 532
integer width = 261
integer height = 84
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
boolean enabled = false
string text = "Mercado"
boolean focusrectangle = false
end type

type st_10 from statictext within w_info_planillainspeccion_destino
integer x = 283
integer y = 808
integer width = 256
integer height = 84
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
boolean enabled = false
string text = "Nave"
boolean focusrectangle = false
end type

type cbx_nave from checkbox within w_info_planillainspeccion_destino
integer x = 1573
integer y = 808
integer width = 101
integer height = 64
integer taborder = 40
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
string text = " "
boolean checked = true
end type

event clicked;Integer	li_Null

SetNull(li_Null)
IF This.Checked THEN
	//dw_nave.Enabled	=	FALSE	
	dw_nave.SetItem(1, "nave_codigo", li_Null)
	//dw_nave.Object.nave_codigo.BackGround.Color	=	RGB(192, 192, 192)
ELSE
	//dw_nave.Enabled	=	TRUE
	dw_nave.Setfocus()
	//dw_nave.Object.nave_codigo.BackGround.Color	=	RGB(255, 255, 255)
END IF
end event

type st_11 from statictext within w_info_planillainspeccion_destino
integer x = 297
integer y = 948
integer width = 283
integer height = 84
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
boolean enabled = false
string text = "Recibidor"
boolean focusrectangle = false
end type

type dw_nave from datawindow within w_info_planillainspeccion_destino
integer x = 654
integer y = 800
integer width = 896
integer height = 84
integer taborder = 50
boolean bringtotop = true
string dataobject = "dddw_navefecar"
boolean border = false
boolean livescroll = true
end type

event itemchanged;string ls_tipotrans

SetNull(is_nula)

ls_tipotrans	= uo_tipotrans.dw_Seleccion.Object.codigo[1]

IF iuo_naves.existe(Integer(Data),ls_tipotrans,True,sqlca) = False THEN
	This.SetItem(1, "nave_codigo", Integer(is_nula))
	RETURN 1
ELSE
	RETURN 0
END IF	

dw_nave.PostEvent(Clicked!)
end event

event itemerror;Return 1
end event

event clicked;cbx_nave.Checked = False
end event

type st_14 from statictext within w_info_planillainspeccion_destino
integer x = 1723
integer y = 536
integer width = 389
integer height = 84
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
boolean enabled = false
string text = "Especie"
boolean focusrectangle = false
end type

type st_1 from statictext within w_info_planillainspeccion_destino
integer x = 1797
integer y = 1412
integer width = 512
integer height = 84
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
string text = " Fecha Embalaje"
boolean focusrectangle = false
end type

type st_3 from statictext within w_info_planillainspeccion_destino
integer x = 1723
integer y = 676
integer width = 389
integer height = 84
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
string text = "Variedad"
boolean focusrectangle = false
end type

type st_5 from statictext within w_info_planillainspeccion_destino
integer x = 283
integer y = 676
integer width = 311
integer height = 84
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
string text = "Tipo Nave"
boolean focusrectangle = false
end type

type ddlb_mercado from dropdownlistbox within w_info_planillainspeccion_destino
integer x = 654
integer y = 520
integer width = 896
integer height = 252
integer taborder = 20
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
boolean vscrollbar = true
string item[] = {"USA","UK"}
borderstyle borderstyle = stylelowered!
end type

event selectionchanged;IF Integer(index) = 1 THEN
	istr_Mant.Argumento[1] = String(3)
ELSEIF Integer(index) = 2 THEN
	istr_Mant.Argumento[1] = String(1)
END IF
uo_muestrarecibidor.Filtra(Integer(Istr_mant.argumento[1]))
Return 0
ddlb_mercado.PostEvent(Clicked!)

end event

event modified;cbx_mercado.Checked = False
end event

type cbx_mercado from checkbox within w_info_planillainspeccion_destino
integer x = 1568
integer y = 536
integer width = 96
integer height = 64
integer taborder = 10
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
string text = " "
boolean checked = true
end type

event clicked;Integer	li_null

SetNull(li_null)
IF This.Checked THEN
	/*Nave*/
	idwc_nave.Retrieve(0,'*')
	
	ddlb_mercado.SelectItem ( 0 )
	/*recibidor*/
   uo_muestrarecibidor.filtra(0)
	//ddlb_mercado.backcolor = RGB(192, 192, 192)
	//ddlb_mercado.enabled = FALSE
ELSE
	ddlb_mercado.SetFocus()
	//ddlb_mercado.enabled = TRUE
	//ddlb_mercado.backcolor = RGB(255, 255, 255)
END IF
return 0

end event

type dw_planilla from datawindow within w_info_planillainspeccion_destino
integer x = 613
integer y = 1492
integer width = 896
integer height = 104
integer taborder = 80
boolean bringtotop = true
string title = "none"
string dataobject = "dddw_nroplanilla"
boolean border = false
boolean livescroll = true
end type

event itemchanged;SetNull(is_nula)

IF iuo_nroplanilla.existe(Long(Data),True,sqlca) = False THEN
	This.SetItem(1, "num_planil", Long(is_nula))
	RETURN 1
ELSE
	RETURN 0
END IF
dw_planilla.PostEvent(Clicked!)
end event

event clicked;cbx_planilla.Checked = False
end event

type cbx_planilla from checkbox within w_info_planillainspeccion_destino
integer x = 1531
integer y = 1508
integer width = 114
integer height = 64
integer taborder = 70
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
string text = " "
boolean checked = true
end type

event clicked;Integer	li_Null

SetNull(li_Null)
IF This.Checked THEN
	//dw_planilla.Enabled	=	FALSE	
	dw_planilla.SetItem(1, "num_planil", li_Null)
	//dw_planilla.Object.num_planil.BackGround.Color	=	RGB(192, 192, 192)
ELSE
	//dw_planilla.Enabled	=	TRUE
	dw_planilla.Setfocus()
	//dw_planilla.Object.num_planil.BackGround.Color	=	RGB(255, 255, 255)
END IF
end event

type gb_3 from groupbox within w_info_planillainspeccion_destino
integer x = 1783
integer y = 1408
integer width = 1376
integer height = 300
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
end type

type st_7 from statictext within w_info_planillainspeccion_destino
integer x = 1737
integer y = 1388
integer width = 1486
integer height = 364
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

type st_2 from statictext within w_info_planillainspeccion_destino
integer x = 1481
integer y = 436
integer width = 224
integer height = 84
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
boolean enabled = false
string text = "Todos"
boolean focusrectangle = false
end type

type uo_muestrarecibidor from uo_seleccion_recibidor_planilla_mod within w_info_planillainspeccion_destino
integer x = 654
integer y = 924
integer width = 1024
integer height = 124
integer taborder = 60
boolean bringtotop = true
end type

on uo_muestrarecibidor.destroy
call uo_seleccion_recibidor_planilla_mod::destroy
end on

type uo_tipotrans from uo_seleccion_tipotransporte_mod within w_info_planillainspeccion_destino
event destroy ( )
integer x = 654
integer y = 644
integer width = 1024
integer taborder = 30
boolean bringtotop = true
end type

on uo_tipotrans.destroy
call uo_seleccion_tipotransporte_mod::destroy
end on

type uo_muestrazona from uo_seleccion_zonas_mod within w_info_planillainspeccion_destino
integer x = 649
integer y = 1052
integer width = 1056
integer taborder = 110
boolean bringtotop = true
end type

event ue_cambio;call super::ue_cambio;IF IsNull(This.Codigo) THEN RETURN

CHOOSE CASE This.Codigo
	CASE -1, -9
		uo_muestraproductor.Todos(True)
		
		uo_muestraproductor.cbx_Todos.Enabled	=	False
		
	CASE ELSE
		uo_muestraproductor.Filtra(This.Codigo)
		
		uo_muestraproductor.cbx_Todos.Enabled	=	True
END CHOOSE
end event

on uo_muestrazona.destroy
call uo_seleccion_zonas_mod::destroy
end on

type uo_muestraespecies from uo_seleccion_especie_mod within w_info_planillainspeccion_destino
event destroy ( )
integer x = 2085
integer y = 500
integer width = 1024
integer height = 124
integer taborder = 90
boolean bringtotop = true
end type

on uo_muestraespecies.destroy
call uo_seleccion_especie_mod::destroy
end on

event ue_cambio;call super::ue_cambio;IF IsNull(This.Codigo) THEN RETURN

CHOOSE CASE This.Codigo
	CASE -1, -9
		//uo_muestravariedad.Todos(True)
		
		//uo_muestravariedad.cbx_Todos.Enabled	=	False
		
	CASE ELSE
		uo_muestravariedad.Filtra(This.Codigo)
		
		//uo_muestravariedad.cbx_Todos.Enabled	=	True
END CHOOSE
end event

type uo_muestraproductor from uo_seleccion_productor_zonas_mod within w_info_planillainspeccion_destino
event destroy ( )
integer x = 654
integer y = 1192
integer width = 1024
integer height = 124
integer taborder = 120
boolean bringtotop = true
end type

on uo_muestraproductor.destroy
call uo_seleccion_productor_zonas_mod::destroy
end on

type uo_muestravariedad from uo_seleccion_variedad_mod within w_info_planillainspeccion_destino
integer x = 2085
integer y = 644
integer width = 1024
integer height = 124
integer taborder = 100
boolean bringtotop = true
end type

on uo_muestravariedad.destroy
call uo_seleccion_variedad_mod::destroy
end on

type st_15 from statictext within w_info_planillainspeccion_destino
integer x = 2935
integer y = 436
integer width = 224
integer height = 84
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
boolean enabled = false
string text = "Todos"
boolean focusrectangle = false
end type

type gb_4 from groupbox within w_info_planillainspeccion_destino
integer x = 283
integer y = 1396
integer width = 1376
integer height = 300
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
string text = "Nº Planilla"
end type

type st_8 from statictext within w_info_planillainspeccion_destino
integer x = 251
integer y = 1388
integer width = 1486
integer height = 364
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

type st_6 from statictext within w_info_planillainspeccion_destino
integer x = 1723
integer y = 808
integer width = 347
integer height = 84
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
string text = "Cód. Emb."
boolean focusrectangle = false
end type

type st_16 from statictext within w_info_planillainspeccion_destino
integer x = 1723
integer y = 948
integer width = 347
integer height = 84
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
string text = "Calibre"
boolean focusrectangle = false
end type

type uo_muestraembalaje from uo_seleccion_embalaje_mod within w_info_planillainspeccion_destino
integer x = 2094
integer y = 776
integer width = 1056
integer taborder = 50
boolean bringtotop = true
end type

on uo_muestraembalaje.destroy
call uo_seleccion_embalaje_mod::destroy
end on

type uo_muestracalibre from uo_seleccion_calibre_mod within w_info_planillainspeccion_destino
integer x = 2094
integer y = 900
integer width = 1047
integer taborder = 70
boolean bringtotop = true
end type

on uo_muestracalibre.destroy
call uo_seleccion_calibre_mod::destroy
end on

type gb_5 from groupbox within w_info_planillainspeccion_destino
integer x = 1719
integer y = 1072
integer width = 1257
integer height = 216
integer taborder = 120
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
string text = "Reclamos"
end type

type st_4 from statictext within w_info_planillainspeccion_destino
integer x = 251
integer y = 428
integer width = 2971
integer height = 956
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

type cbx_todosrec from checkbox within w_info_planillainspeccion_destino
integer x = 2999
integer y = 1148
integer width = 119
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
boolean checked = true
end type

event clicked;IF This.Checked THEN 
	ii_Reclamo = -1
ELSE
	ii_Reclamo = 0
END IF
rb_no.Checked = FALSE
rb_si.Checked = FALSE
end event

type rb_si from radiobutton within w_info_planillainspeccion_destino
integer x = 1929
integer y = 1172
integer width = 224
integer height = 72
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 33543637
string text = "SI"
boolean lefttext = true
end type

event clicked;ii_Reclamo = 1
rb_no.Checked = FALSE
cbx_todosrec.Checked = FALSE
end event

type rb_no from radiobutton within w_info_planillainspeccion_destino
integer x = 2514
integer y = 1168
integer width = 251
integer height = 72
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 33543637
string text = "NO"
boolean lefttext = true
end type

event clicked;ii_Reclamo = 2
rb_si.Checked = FALSE
cbx_todosrec.Checked = FALSE
end event

