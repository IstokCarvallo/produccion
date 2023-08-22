$PBExportHeader$w_info_llegada_destino_porprod.srw
$PBExportComments$Ventana de Consulta planilla de recepcion
forward
global type w_info_llegada_destino_porprod from w_para_informes
end type
type em_fechadesde from editmask within w_info_llegada_destino_porprod
end type
type st_12 from statictext within w_info_llegada_destino_porprod
end type
type st_13 from statictext within w_info_llegada_destino_porprod
end type
type em_fechahasta from editmask within w_info_llegada_destino_porprod
end type
type st_zona from statictext within w_info_llegada_destino_porprod
end type
type cbx_todosfecha from checkbox within w_info_llegada_destino_porprod
end type
type st_14 from statictext within w_info_llegada_destino_porprod
end type
type cbx_consfecha from checkbox within w_info_llegada_destino_porprod
end type
type st_2 from statictext within w_info_llegada_destino_porprod
end type
type uo_muestrazona from uo_seleccion_zonas_mod within w_info_llegada_destino_porprod
end type
type uo_muestraespecies from uo_seleccion_especie_mod within w_info_llegada_destino_porprod
end type
type st_1 from statictext within w_info_llegada_destino_porprod
end type
type st_4 from statictext within w_info_llegada_destino_porprod
end type
type uo_muestravariedad from uo_seleccion_variedad_mod within w_info_llegada_destino_porprod
end type
type st_3 from statictext within w_info_llegada_destino_porprod
end type
type st_8 from statictext within w_info_llegada_destino_porprod
end type
type gb_3 from groupbox within w_info_llegada_destino_porprod
end type
type uo_muestramercado from uo_seleccion_mercado_mod within w_info_llegada_destino_porprod
end type
type st_5 from statictext within w_info_llegada_destino_porprod
end type
type st_6 from statictext within w_info_llegada_destino_porprod
end type
type st_7 from statictext within w_info_llegada_destino_porprod
end type
type st_9 from statictext within w_info_llegada_destino_porprod
end type
type st_10 from statictext within w_info_llegada_destino_porprod
end type
type st_11 from statictext within w_info_llegada_destino_porprod
end type
type gb_4 from groupbox within w_info_llegada_destino_porprod
end type
type st_44 from statictext within w_info_llegada_destino_porprod
end type
type uo_muestraprod from uo_seleccion_productor_zonas_mod within w_info_llegada_destino_porprod
end type
type uo_muestranave from uo_seleccion_naves_mod within w_info_llegada_destino_porprod
end type
type uo_muestrarecibidor from uo_seleccion_recibidor_planilla_mod within w_info_llegada_destino_porprod
end type
type ddlb_calidad from dropdownlistbox within w_info_llegada_destino_porprod
end type
type ddlb_condicion from dropdownlistbox within w_info_llegada_destino_porprod
end type
type ddlb_guarda from dropdownlistbox within w_info_llegada_destino_porprod
end type
type cbx_todosca from checkbox within w_info_llegada_destino_porprod
end type
type cbx_todosco from checkbox within w_info_llegada_destino_porprod
end type
type cbx_todosgu from checkbox within w_info_llegada_destino_porprod
end type
end forward

global type w_info_llegada_destino_porprod from w_para_informes
string tag = "Llegada a destino por productor"
integer x = 14
integer y = 32
integer width = 2523
integer height = 2564
string icon = "F:\Desarrollo\Producción\FrutaProcesada\Producc.ico"
em_fechadesde em_fechadesde
st_12 st_12
st_13 st_13
em_fechahasta em_fechahasta
st_zona st_zona
cbx_todosfecha cbx_todosfecha
st_14 st_14
cbx_consfecha cbx_consfecha
st_2 st_2
uo_muestrazona uo_muestrazona
uo_muestraespecies uo_muestraespecies
st_1 st_1
st_4 st_4
uo_muestravariedad uo_muestravariedad
st_3 st_3
st_8 st_8
gb_3 gb_3
uo_muestramercado uo_muestramercado
st_5 st_5
st_6 st_6
st_7 st_7
st_9 st_9
st_10 st_10
st_11 st_11
gb_4 gb_4
st_44 st_44
uo_muestraprod uo_muestraprod
uo_muestranave uo_muestranave
uo_muestrarecibidor uo_muestrarecibidor
ddlb_calidad ddlb_calidad
ddlb_condicion ddlb_condicion
ddlb_guarda ddlb_guarda
cbx_todosca cbx_todosca
cbx_todosco cbx_todosco
cbx_todosgu cbx_todosgu
end type
global w_info_llegada_destino_porprod w_info_llegada_destino_porprod

type variables
str_busqueda      istr_busq
str_mant         istr_mant
Integer	        ii_tipo
String	        is_report, is_nula


end variables

on w_info_llegada_destino_porprod.create
int iCurrent
call super::create
this.em_fechadesde=create em_fechadesde
this.st_12=create st_12
this.st_13=create st_13
this.em_fechahasta=create em_fechahasta
this.st_zona=create st_zona
this.cbx_todosfecha=create cbx_todosfecha
this.st_14=create st_14
this.cbx_consfecha=create cbx_consfecha
this.st_2=create st_2
this.uo_muestrazona=create uo_muestrazona
this.uo_muestraespecies=create uo_muestraespecies
this.st_1=create st_1
this.st_4=create st_4
this.uo_muestravariedad=create uo_muestravariedad
this.st_3=create st_3
this.st_8=create st_8
this.gb_3=create gb_3
this.uo_muestramercado=create uo_muestramercado
this.st_5=create st_5
this.st_6=create st_6
this.st_7=create st_7
this.st_9=create st_9
this.st_10=create st_10
this.st_11=create st_11
this.gb_4=create gb_4
this.st_44=create st_44
this.uo_muestraprod=create uo_muestraprod
this.uo_muestranave=create uo_muestranave
this.uo_muestrarecibidor=create uo_muestrarecibidor
this.ddlb_calidad=create ddlb_calidad
this.ddlb_condicion=create ddlb_condicion
this.ddlb_guarda=create ddlb_guarda
this.cbx_todosca=create cbx_todosca
this.cbx_todosco=create cbx_todosco
this.cbx_todosgu=create cbx_todosgu
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.em_fechadesde
this.Control[iCurrent+2]=this.st_12
this.Control[iCurrent+3]=this.st_13
this.Control[iCurrent+4]=this.em_fechahasta
this.Control[iCurrent+5]=this.st_zona
this.Control[iCurrent+6]=this.cbx_todosfecha
this.Control[iCurrent+7]=this.st_14
this.Control[iCurrent+8]=this.cbx_consfecha
this.Control[iCurrent+9]=this.st_2
this.Control[iCurrent+10]=this.uo_muestrazona
this.Control[iCurrent+11]=this.uo_muestraespecies
this.Control[iCurrent+12]=this.st_1
this.Control[iCurrent+13]=this.st_4
this.Control[iCurrent+14]=this.uo_muestravariedad
this.Control[iCurrent+15]=this.st_3
this.Control[iCurrent+16]=this.st_8
this.Control[iCurrent+17]=this.gb_3
this.Control[iCurrent+18]=this.uo_muestramercado
this.Control[iCurrent+19]=this.st_5
this.Control[iCurrent+20]=this.st_6
this.Control[iCurrent+21]=this.st_7
this.Control[iCurrent+22]=this.st_9
this.Control[iCurrent+23]=this.st_10
this.Control[iCurrent+24]=this.st_11
this.Control[iCurrent+25]=this.gb_4
this.Control[iCurrent+26]=this.st_44
this.Control[iCurrent+27]=this.uo_muestraprod
this.Control[iCurrent+28]=this.uo_muestranave
this.Control[iCurrent+29]=this.uo_muestrarecibidor
this.Control[iCurrent+30]=this.ddlb_calidad
this.Control[iCurrent+31]=this.ddlb_condicion
this.Control[iCurrent+32]=this.ddlb_guarda
this.Control[iCurrent+33]=this.cbx_todosca
this.Control[iCurrent+34]=this.cbx_todosco
this.Control[iCurrent+35]=this.cbx_todosgu
end on

on w_info_llegada_destino_porprod.destroy
call super::destroy
destroy(this.em_fechadesde)
destroy(this.st_12)
destroy(this.st_13)
destroy(this.em_fechahasta)
destroy(this.st_zona)
destroy(this.cbx_todosfecha)
destroy(this.st_14)
destroy(this.cbx_consfecha)
destroy(this.st_2)
destroy(this.uo_muestrazona)
destroy(this.uo_muestraespecies)
destroy(this.st_1)
destroy(this.st_4)
destroy(this.uo_muestravariedad)
destroy(this.st_3)
destroy(this.st_8)
destroy(this.gb_3)
destroy(this.uo_muestramercado)
destroy(this.st_5)
destroy(this.st_6)
destroy(this.st_7)
destroy(this.st_9)
destroy(this.st_10)
destroy(this.st_11)
destroy(this.gb_4)
destroy(this.st_44)
destroy(this.uo_muestraprod)
destroy(this.uo_muestranave)
destroy(this.uo_muestrarecibidor)
destroy(this.ddlb_calidad)
destroy(this.ddlb_condicion)
destroy(this.ddlb_guarda)
destroy(this.cbx_todosca)
destroy(this.cbx_todosco)
destroy(this.cbx_todosgu)
end on

event open;call super::open;x = 0
y = 0
This.Icon	=	Gstr_apl.Icono

Boolean	lb_Cerrar
/*
Istr_mant.argumento[1]: Calidad
Istr_mant.argumento[2]: Condición
Istr_mant.argumento[3]: Guarda
*/

IF IsNull(uo_muestraespecies.Codigo) THEN lb_Cerrar	=	True

IF IsNull(uo_muestravariedad.Codigo) THEN lb_Cerrar	=	True

IF IsNull(uo_muestrazona.Codigo) THEN lb_Cerrar	=	True

IF IsNull(uo_muestramercado.Codigo) THEN lb_Cerrar	=	True

IF IsNull(uo_muestraprod.Codigo) THEN lb_Cerrar	=	True

IF IsNull(uo_muestranave.Codigo) THEN lb_Cerrar	=	True

IF IsNull(uo_muestrarecibidor.Codigo) THEN lb_Cerrar	=	True

IF lb_Cerrar THEN
	Close(This)
ELSE
	uo_muestraespecies.Seleccion(True, True)
	uo_muestravariedad.Seleccion(True, True)
	uo_muestrazona.Seleccion(True, True)
	uo_muestramercado.Seleccion(True, True)
	uo_muestraprod.Seleccion(True, True)
	uo_muestranave.Seleccion(True, True)
	uo_muestrarecibidor.Seleccion(True, True)
	
	//Especie
	uo_muestraespecies.cbx_todos.checked     = FALSE
	uo_muestraespecies.cbx_todos.visible     = TRUE
	uo_muestraespecies.cbx_consolida.visible = TRUE
	uo_muestraespecies.dw_seleccion.enabled  = TRUE
	uo_muestraespecies.dw_seleccion.object.codigo[1] = 11 
	uo_muestravariedad.Filtra(11)
		
	//Variedad
	uo_muestravariedad.cbx_todos.checked     = TRUE
	uo_muestravariedad.cbx_consolida.checked = FALSE
	uo_muestravariedad.dw_seleccion.enabled  = TRUE
	
	//Zona
	uo_muestrazona.cbx_todos.checked     = TRUE
	uo_muestrazona.cbx_consolida.checked = FALSE
	uo_muestrazona.dw_seleccion.enabled  = TRUE
	
	//Mercado
	uo_muestramercado.cbx_todos.checked     = FALSE
	uo_muestramercado.cbx_consolida.checked = TRUE
	uo_muestramercado.dw_seleccion.enabled  = TRUE
	
	//Productor
	uo_muestraprod.cbx_todos.checked     = TRUE
	uo_muestraprod.cbx_consolida.checked = FALSE
	uo_muestraprod.dw_seleccion.enabled  = TRUE
	
	//Nave
	uo_muestranave.cbx_todos.checked     = TRUE
	uo_muestranave.cbx_consolida.checked = FALSE
	uo_muestranave.dw_seleccion.enabled  = TRUE
	
	//Recibidor
	uo_muestrarecibidor.cbx_todos.checked     = FALSE
	uo_muestrarecibidor.cbx_consolida.checked = TRUE
	uo_muestrarecibidor.dw_seleccion.enabled  = TRUE
	
	em_fechadesde.text	  = String(Today())
	em_fechahasta.text	  = String(Today())
END IF
Istr_mant.argumento[1] = ""
Istr_mant.argumento[2] = ""
Istr_mant.argumento[3] = ""
end event

type st_computador from w_para_informes`st_computador within w_info_llegada_destino_porprod
end type

type st_usuario from w_para_informes`st_usuario within w_info_llegada_destino_porprod
end type

type st_temporada from w_para_informes`st_temporada within w_info_llegada_destino_porprod
end type

type p_logo from w_para_informes`p_logo within w_info_llegada_destino_porprod
end type

type st_titulo from w_para_informes`st_titulo within w_info_llegada_destino_porprod
integer width = 1833
string text = "Llegada a Destino por Productor"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_llegada_destino_porprod
string tag = "Imprimir Reporte"
integer x = 2194
integer y = 512
integer taborder = 190
integer weight = 400
fontcharset fontcharset = ansi!
string powertiptext = "Imprimir Reporte"
end type

event pb_acepta::clicked;Integer	li_fila
Date		ld_Fechadesde, ld_Fechahasta

If cbx_todosca.Checked Then Istr_mant.argumento[1] = '-1'
If cbx_todosco.Checked Then Istr_mant.argumento[2] = '-1'
If cbx_todosgu.Checked Then Istr_mant.argumento[3] = '-1'

//Fecha Arribo
If cbx_consfecha.Checked Then
	ld_Fechadesde =	Date(01/01/2000)
	ld_Fechahasta =	Today()
ElseIf cbx_todosfecha.Checked Then
  ld_Fechadesde =	Date(01/01/2000)
  ld_Fechahasta =	Today()
  em_fechadesde.text = String(ld_Fechadesde,"dd/mm/yyyy")
  em_fechahasta.Text = String(ld_Fechahasta,"dd/mm/yyyy")
Else
 ld_Fechadesde = Date(em_fechadesde.Text)
 ld_Fechahasta = Date(em_fechahasta.Text)
End If

OpenWithParm(vinf,istr_info)
istr_info.titulo	= 'INFORME LLEGADA A DESTINO POR PRODUCTOR'
vinf.dw_1.DataObject = "dw_info_llegada_destino_por_productor"
vinf.dw_1.Object.DataWindow.Zoom = 75

vinf.dw_1.SetTransObject(sqlca)	
li_fila = vinf.dw_1.Retrieve(uo_MuestraZona.Codigo,uo_MuestraEspecies.Codigo,uo_MuestraVariedad.Codigo,uo_MuestraProd.Codigo,uo_MuestraMercado.Codigo,&
									uo_MuestraNave.Codigo,uo_MuestraRecibidor.Codigo,Integer(istr_mant.argumento[1]),Integer(istr_mant.argumento[2]),&
									Integer(istr_mant.argumento[3]),ld_Fechadesde,ld_Fechahasta)
										  
If li_fila = -1 Then
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ElseIf li_fila = 0 Then
	MessageBox( "No Existe información", "No existe información para este informe.", StopSign!, Ok!)
Else
	F_Membrete(vinf.dw_1)
	If gs_Ambiente <> 'Windows' Then F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
End If

end event

type pb_salir from w_para_informes`pb_salir within w_info_llegada_destino_porprod
string tag = "Salir [Cerrar Ventana Activa]"
integer x = 2190
integer y = 792
integer taborder = 200
string powertiptext = "Salir [Cerrar Ventana Activa]"
end type

type em_fechadesde from editmask within w_info_llegada_destino_porprod
integer x = 645
integer y = 2084
integer width = 407
integer height = 92
integer taborder = 110
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

type st_12 from statictext within w_info_llegada_destino_porprod
integer x = 416
integer y = 2096
integer width = 215
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
string text = "Desde"
boolean focusrectangle = false
end type

type st_13 from statictext within w_info_llegada_destino_porprod
integer x = 1065
integer y = 2096
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

type em_fechahasta from editmask within w_info_llegada_destino_porprod
integer x = 1239
integer y = 2084
integer width = 407
integer height = 92
integer taborder = 120
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

type st_zona from statictext within w_info_llegada_destino_porprod
integer x = 306
integer y = 600
integer width = 370
integer height = 84
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

type cbx_todosfecha from checkbox within w_info_llegada_destino_porprod
integer x = 1659
integer y = 2096
integer width = 123
integer height = 64
integer taborder = 100
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
	//cbx_consfecha.enabled   =  TRUE
	cbx_consfecha.checked   =  FALSE
ELSE
	em_fechadesde.Enabled	=	TRUE
	em_fechahasta.Enabled	=	TRUE
	em_fechadesde.SetFocus()
	//cbx_consfecha.enabled   =  FALSE
END IF
RETURN 0



end event

type st_14 from statictext within w_info_llegada_destino_porprod
integer x = 306
integer y = 740
integer width = 370
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

type cbx_consfecha from checkbox within w_info_llegada_destino_porprod
integer x = 1851
integer y = 2096
integer width = 91
integer height = 64
integer taborder = 90
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
end type

event clicked;IF This.Checked THEN
	em_fechadesde.Enabled	=	FALSE
	em_fechahasta.Enabled	=	FALSE
   istr_Mant.Argumento[12]	=	'01/01/1900'
	istr_Mant.Argumento[13]	=	String(Today())
	//cbx_consfecha.enabled   =  TRUE
	cbx_todosfecha.Checked = False
ELSE
	em_fechadesde.Enabled	=	TRUE
	em_fechahasta.Enabled	=	TRUE
	em_fechadesde.SetFocus()
	//cbx_consfecha.enabled   =  FALSE
END IF
RETURN 0
end event

type st_2 from statictext within w_info_llegada_destino_porprod
integer x = 1586
integer y = 488
integer width = 197
integer height = 60
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

type uo_muestrazona from uo_seleccion_zonas_mod within w_info_llegada_destino_porprod
integer x = 745
integer y = 584
integer width = 1262
integer height = 112
integer taborder = 130
boolean bringtotop = true
end type

on uo_muestrazona.destroy
call uo_seleccion_zonas_mod::destroy
end on

event ue_cambio;call super::ue_cambio;//IF IsNull(This.Codigo) THEN RETURN
//
//CHOOSE CASE This.Codigo
//	CASE -1, -9
//		uo_muestrainspector.Todos(True)
//		uo_muestrainspector.cbx_Todos.Enabled	=	False
//		
//	CASE ELSE
//		
//		uo_muestrainspector.Filtra(This.Codigo)
//		uo_muestrainspector.cbx_Todos.Enabled	=	True
//END CHOOSE
end event

type uo_muestraespecies from uo_seleccion_especie_mod within w_info_llegada_destino_porprod
event destroy ( )
integer x = 745
integer y = 708
integer width = 1221
integer height = 112
integer taborder = 150
boolean bringtotop = true
end type

on uo_muestraespecies.destroy
call uo_seleccion_especie_mod::destroy
end on

event ue_cambio;call super::ue_cambio;IF IsNull(This.Codigo) THEN RETURN

CHOOSE CASE This.Codigo
	CASE -1, -9
		uo_muestravariedad.Todos(True)
			
		uo_muestravariedad.cbx_Todos.Enabled	=	False
				
	CASE ELSE
		uo_muestravariedad.Filtra(This.Codigo)
					
		uo_muestravariedad.cbx_Todos.Enabled	=	True
		
END CHOOSE
end event

type st_1 from statictext within w_info_llegada_destino_porprod
integer x = 306
integer y = 1104
integer width = 370
integer height = 72
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
string text = "Mercado"
boolean focusrectangle = false
end type

type st_4 from statictext within w_info_llegada_destino_porprod
integer x = 293
integer y = 1996
integer width = 416
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
string text = "Fecha Arribo"
boolean focusrectangle = false
end type

type uo_muestravariedad from uo_seleccion_variedad_mod within w_info_llegada_destino_porprod
integer x = 745
integer y = 828
integer width = 1230
integer height = 112
integer taborder = 160
boolean bringtotop = true
end type

on uo_muestravariedad.destroy
call uo_seleccion_variedad_mod::destroy
end on

type st_3 from statictext within w_info_llegada_destino_porprod
integer x = 306
integer y = 868
integer width = 370
integer height = 64
boolean bringtotop = true
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

type st_8 from statictext within w_info_llegada_destino_porprod
integer x = 1838
integer y = 488
integer width = 201
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
string text = "Cons."
boolean focusrectangle = false
end type

type gb_3 from groupbox within w_info_llegada_destino_porprod
integer x = 293
integer y = 1992
integer width = 1769
integer height = 228
integer taborder = 150
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
end type

type uo_muestramercado from uo_seleccion_mercado_mod within w_info_llegada_destino_porprod
integer x = 745
integer y = 1072
integer taborder = 170
boolean bringtotop = true
end type

on uo_muestramercado.destroy
call uo_seleccion_mercado_mod::destroy
end on

type st_5 from statictext within w_info_llegada_destino_porprod
integer x = 306
integer y = 984
integer width = 370
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
string text = "Productor"
boolean focusrectangle = false
end type

type st_6 from statictext within w_info_llegada_destino_porprod
integer x = 306
integer y = 1224
integer width = 370
integer height = 72
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
string text = "Nave"
boolean focusrectangle = false
end type

type st_7 from statictext within w_info_llegada_destino_porprod
integer x = 306
integer y = 1352
integer width = 370
integer height = 72
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
string text = "Recibidor"
boolean focusrectangle = false
end type

type st_9 from statictext within w_info_llegada_destino_porprod
integer x = 306
integer y = 1568
integer width = 320
integer height = 72
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
string text = "Calidad"
boolean focusrectangle = false
end type

type st_10 from statictext within w_info_llegada_destino_porprod
integer x = 311
integer y = 1684
integer width = 370
integer height = 72
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
string text = "Condición"
boolean focusrectangle = false
end type

type st_11 from statictext within w_info_llegada_destino_porprod
integer x = 306
integer y = 1800
integer width = 288
integer height = 72
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
string text = "Guarda"
boolean focusrectangle = false
end type

type gb_4 from groupbox within w_info_llegada_destino_porprod
integer x = 288
integer y = 1468
integer width = 1769
integer height = 480
integer taborder = 180
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 33543637
end type

type st_44 from statictext within w_info_llegada_destino_porprod
integer x = 251
integer y = 440
integer width = 1833
integer height = 1888
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

type uo_muestraprod from uo_seleccion_productor_zonas_mod within w_info_llegada_destino_porprod
integer x = 745
integer y = 948
integer taborder = 170
boolean bringtotop = true
end type

on uo_muestraprod.destroy
call uo_seleccion_productor_zonas_mod::destroy
end on

type uo_muestranave from uo_seleccion_naves_mod within w_info_llegada_destino_porprod
integer x = 745
integer y = 1192
integer taborder = 180
boolean bringtotop = true
end type

on uo_muestranave.destroy
call uo_seleccion_naves_mod::destroy
end on

type uo_muestrarecibidor from uo_seleccion_recibidor_planilla_mod within w_info_llegada_destino_porprod
integer x = 745
integer y = 1316
integer taborder = 190
boolean bringtotop = true
end type

on uo_muestrarecibidor.destroy
call uo_seleccion_recibidor_planilla_mod::destroy
end on

type ddlb_calidad from dropdownlistbox within w_info_llegada_destino_porprod
integer x = 750
integer y = 1556
integer width = 480
integer height = 404
integer taborder = 190
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
string item[] = {"Buena","Regular","Mala"}
borderstyle borderstyle = stylelowered!
end type

event selectionchanged;IF Index = 1 THEN
	Istr_mant.argumento[1] = '1'
ELSEIF Index = 2 THEN
	Istr_mant.argumento[1] = '2'
ELSE 
	Istr_mant.argumento[1] = '3'
END IF
end event

type ddlb_condicion from dropdownlistbox within w_info_llegada_destino_porprod
integer x = 750
integer y = 1676
integer width = 480
integer height = 400
integer taborder = 200
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
string item[] = {"Buena ","Regular","Mala"}
borderstyle borderstyle = stylelowered!
end type

event selectionchanged;IF Index = 1 THEN
	Istr_mant.argumento[2] = '1'
ELSEIF Index = 2 THEN
	Istr_mant.argumento[2] = '2'
ELSE 
	Istr_mant.argumento[2] = '3'
END IF
end event

type ddlb_guarda from dropdownlistbox within w_info_llegada_destino_porprod
integer x = 745
integer y = 1796
integer width = 480
integer height = 400
integer taborder = 210
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
string item[] = {"Prolongada","Media","S/Guarda"}
borderstyle borderstyle = stylelowered!
end type

event selectionchanged;IF Index = 1 THEN
	Istr_mant.argumento[3] = '1'
ELSEIF Index = 2 THEN
	Istr_mant.argumento[3] = '2'
ELSE 
	Istr_mant.argumento[3] = '3'
END IF
end event

type cbx_todosca from checkbox within w_info_llegada_destino_porprod
integer x = 1664
integer y = 1548
integer width = 96
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
	ddlb_calidad.Enabled = FALSE
ELSE
	ddlb_calidad.Enabled = TRUE
END IF
end event

type cbx_todosco from checkbox within w_info_llegada_destino_porprod
integer x = 1664
integer y = 1672
integer width = 96
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
	ddlb_condicion.Enabled = FALSE
ELSE
	ddlb_condicion.Enabled = TRUE
END IF
end event

type cbx_todosgu from checkbox within w_info_llegada_destino_porprod
integer x = 1664
integer y = 1800
integer width = 96
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
	ddlb_guarda.Enabled = FALSE
ELSE
	ddlb_guarda.Enabled = TRUE
END IF
end event

