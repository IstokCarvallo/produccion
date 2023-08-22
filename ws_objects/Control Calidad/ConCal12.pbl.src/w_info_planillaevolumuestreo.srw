$PBExportHeader$w_info_planillaevolumuestreo.srw
$PBExportComments$Ventana de Consulta planilla de recepcion
forward
global type w_info_planillaevolumuestreo from w_para_informes
end type
type em_fechadesde from editmask within w_info_planillaevolumuestreo
end type
type st_12 from statictext within w_info_planillaevolumuestreo
end type
type st_13 from statictext within w_info_planillaevolumuestreo
end type
type em_fechahasta from editmask within w_info_planillaevolumuestreo
end type
type st_zona from statictext within w_info_planillaevolumuestreo
end type
type st_33 from statictext within w_info_planillaevolumuestreo
end type
type cbx_todosfecha from checkbox within w_info_planillaevolumuestreo
end type
type st_14 from statictext within w_info_planillaevolumuestreo
end type
type st_2 from statictext within w_info_planillaevolumuestreo
end type
type uo_muestrazona from uo_seleccion_zonas_mod within w_info_planillaevolumuestreo
end type
type uo_muestraespecies from uo_seleccion_especie_mod within w_info_planillaevolumuestreo
end type
type st_1 from statictext within w_info_planillaevolumuestreo
end type
type st_4 from statictext within w_info_planillaevolumuestreo
end type
type st_8 from statictext within w_info_planillaevolumuestreo
end type
type gb_3 from groupbox within w_info_planillaevolumuestreo
end type
type uo_muestraproductor from uo_seleccion_productor_mod within w_info_planillaevolumuestreo
end type
type gb_5 from groupbox within w_info_planillaevolumuestreo
end type
type st_44 from statictext within w_info_planillaevolumuestreo
end type
type ddlb_informe from dropdownlistbox within w_info_planillaevolumuestreo
end type
type uo_muestraplanta from uo_seleccion_frigopacking_mod within w_info_planillaevolumuestreo
end type
type cbx_dia from checkbox within w_info_planillaevolumuestreo
end type
type cbx_semana from checkbox within w_info_planillaevolumuestreo
end type
type cbx_quincena from checkbox within w_info_planillaevolumuestreo
end type
end forward

global type w_info_planillaevolumuestreo from w_para_informes
string tag = "Consulta Planilla - Evolucion de muestreo"
integer x = 14
integer y = 32
integer width = 2546
integer height = 1672
string icon = "F:\Desarrollo\Producción\FrutaProcesada\Producc.ico"
em_fechadesde em_fechadesde
st_12 st_12
st_13 st_13
em_fechahasta em_fechahasta
st_zona st_zona
st_33 st_33
cbx_todosfecha cbx_todosfecha
st_14 st_14
st_2 st_2
uo_muestrazona uo_muestrazona
uo_muestraespecies uo_muestraespecies
st_1 st_1
st_4 st_4
st_8 st_8
gb_3 gb_3
uo_muestraproductor uo_muestraproductor
gb_5 gb_5
st_44 st_44
ddlb_informe ddlb_informe
uo_muestraplanta uo_muestraplanta
cbx_dia cbx_dia
cbx_semana cbx_semana
cbx_quincena cbx_quincena
end type
global w_info_planillaevolumuestreo w_info_planillaevolumuestreo

type variables
str_busqueda      istr_busq
str_mant         istr_mant
Integer	        ii_tipo
String	        is_report, is_nula, is_informe
uo_Especie     iuo_Especie


end variables

on w_info_planillaevolumuestreo.create
int iCurrent
call super::create
this.em_fechadesde=create em_fechadesde
this.st_12=create st_12
this.st_13=create st_13
this.em_fechahasta=create em_fechahasta
this.st_zona=create st_zona
this.st_33=create st_33
this.cbx_todosfecha=create cbx_todosfecha
this.st_14=create st_14
this.st_2=create st_2
this.uo_muestrazona=create uo_muestrazona
this.uo_muestraespecies=create uo_muestraespecies
this.st_1=create st_1
this.st_4=create st_4
this.st_8=create st_8
this.gb_3=create gb_3
this.uo_muestraproductor=create uo_muestraproductor
this.gb_5=create gb_5
this.st_44=create st_44
this.ddlb_informe=create ddlb_informe
this.uo_muestraplanta=create uo_muestraplanta
this.cbx_dia=create cbx_dia
this.cbx_semana=create cbx_semana
this.cbx_quincena=create cbx_quincena
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.em_fechadesde
this.Control[iCurrent+2]=this.st_12
this.Control[iCurrent+3]=this.st_13
this.Control[iCurrent+4]=this.em_fechahasta
this.Control[iCurrent+5]=this.st_zona
this.Control[iCurrent+6]=this.st_33
this.Control[iCurrent+7]=this.cbx_todosfecha
this.Control[iCurrent+8]=this.st_14
this.Control[iCurrent+9]=this.st_2
this.Control[iCurrent+10]=this.uo_muestrazona
this.Control[iCurrent+11]=this.uo_muestraespecies
this.Control[iCurrent+12]=this.st_1
this.Control[iCurrent+13]=this.st_4
this.Control[iCurrent+14]=this.st_8
this.Control[iCurrent+15]=this.gb_3
this.Control[iCurrent+16]=this.uo_muestraproductor
this.Control[iCurrent+17]=this.gb_5
this.Control[iCurrent+18]=this.st_44
this.Control[iCurrent+19]=this.ddlb_informe
this.Control[iCurrent+20]=this.uo_muestraplanta
this.Control[iCurrent+21]=this.cbx_dia
this.Control[iCurrent+22]=this.cbx_semana
this.Control[iCurrent+23]=this.cbx_quincena
end on

on w_info_planillaevolumuestreo.destroy
call super::destroy
destroy(this.em_fechadesde)
destroy(this.st_12)
destroy(this.st_13)
destroy(this.em_fechahasta)
destroy(this.st_zona)
destroy(this.st_33)
destroy(this.cbx_todosfecha)
destroy(this.st_14)
destroy(this.st_2)
destroy(this.uo_muestrazona)
destroy(this.uo_muestraespecies)
destroy(this.st_1)
destroy(this.st_4)
destroy(this.st_8)
destroy(this.gb_3)
destroy(this.uo_muestraproductor)
destroy(this.gb_5)
destroy(this.st_44)
destroy(this.ddlb_informe)
destroy(this.uo_muestraplanta)
destroy(this.cbx_dia)
destroy(this.cbx_semana)
destroy(this.cbx_quincena)
end on

event open;call super::open;/* =====> Argumentos <===== */
/* 1 ==> Tipo Informe       */

x = 0
y = 0
This.Icon	=	Gstr_apl.Icono

Boolean	lb_Cerrar

iuo_Especie = CREATE uo_Especie

IF IsNull(uo_muestraespecies.Codigo) THEN lb_Cerrar	=	True

IF IsNull(uo_muestrazona.Codigo) THEN lb_Cerrar	=	True

IF IsNull(uo_muestraproductor.Codigo) THEN lb_Cerrar	=	True

IF lb_Cerrar THEN
	Close(This)
ELSE
	uo_muestraespecies.Seleccion(True, True)
	uo_muestrazona.Seleccion(True, True)
	uo_muestraproductor.Seleccion(True, True)	
	
   	istr_mant.argumento[1] = '7'
	Istr_Mant.Argumento[2] = 'Semanal'
	//Especie
	uo_muestraespecies.cbx_todos.checked     = FALSE
	uo_muestraespecies.cbx_todos.visible     = FALSE
	uo_muestraespecies.cbx_consolida.visible = FALSE
	uo_muestraespecies.dw_seleccion.enabled  = TRUE
	uo_muestraespecies.dw_seleccion.object.codigo[1] = 11
	uo_muestraespecies.dw_seleccion.enabled  = TRUE	
	
	//Zona
	uo_muestrazona.cbx_todos.checked     = TRUE
	uo_muestrazona.cbx_consolida.checked = FALSE
	uo_muestrazona.dw_seleccion.enabled  = TRUE
	
	//Planta
	uo_muestraplanta.cbx_todos.checked     = TRUE
	uo_muestraplanta.cbx_consolida.checked = FALSE
	uo_muestraplanta.dw_seleccion.enabled  = TRUE
	
	//Productor
	uo_muestraproductor.cbx_consolida.checked = TRUE
	uo_muestraproductor.cbx_todos.checked     = FALSE
	uo_muestraproductor.dw_seleccion.enabled  = TRUE
	uo_muestraproductor.filtra(0)	
	
	em_fechadesde.text	  = String(Today())
	em_fechahasta.text	  = String(Today())
END IF
end event

type pb_excel from w_para_informes`pb_excel within w_info_planillaevolumuestreo
end type

type st_computador from w_para_informes`st_computador within w_info_planillaevolumuestreo
end type

type st_usuario from w_para_informes`st_usuario within w_info_planillaevolumuestreo
end type

type st_temporada from w_para_informes`st_temporada within w_info_planillaevolumuestreo
end type

type p_logo from w_para_informes`p_logo within w_info_planillaevolumuestreo
end type

type st_titulo from w_para_informes`st_titulo within w_info_planillaevolumuestreo
integer x = 270
integer y = 260
integer width = 1833
string text = "Evolución Nivel de Muestreo"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_planillaevolumuestreo
string tag = "Imprimir Reporte"
integer x = 2231
integer y = 388
integer taborder = 190
end type

event pb_acepta::clicked;Integer	li_fila, li_zona, li_especie, li_planta ,mm 
Date		ld_FechaEmbaini, ld_FechaEmbafin
Long     ll_Productor
String   ls_especie

SetPointer(HourGlass!)

//Especies
li_especie	= uo_muestraespecies.dw_Seleccion.Object.codigo[1]
iuo_Especie.Existe(li_especie,False,SqlCa)
IF IsNull(li_especie)THEN
   MessageBox("Atención","Debe Seleccionar una Especie Previamente",Exclamation!)
	RETURN
END IF

//zona
IF uo_muestrazona.cbx_consolida.checked THEN
	li_zona  = -9
ELSE
   IF uo_muestrazona.cbx_todos.checked THEN
	   li_zona	= -1
   ELSE
      li_zona	= uo_muestrazona.dw_Seleccion.Object.codigo[1]
	   IF IsNull(li_zona)THEN
	      MessageBox("Atención","Debe Seleccionar una Zona Previamente",Exclamation!)
	      RETURN
	   END IF
   END IF
END IF

//Planta
IF uo_muestraplanta.cbx_consolida.checked THEN
	li_planta  = -9
ELSE
   IF uo_muestraplanta.cbx_todos.checked THEN
	   li_planta	= -1
   ELSE
      li_planta	= uo_muestraplanta.dw_Seleccion.Object.codigo[1]
	   IF IsNull(li_planta)THEN
	      MessageBox("Atención","Debe Seleccionar Packing Previamente",Exclamation!)
	      RETURN
	   END IF
   END IF
END IF

//Productor
IF uo_muestraproductor.cbx_consolida.checked THEN
	ll_productor = -9
ELSE
   IF uo_muestraproductor.cbx_todos.checked THEN
	   ll_productor = -1
   ELSE
      ll_productor = uo_muestraproductor.dw_Seleccion.Object.codigo[1]
	   IF IsNull(ll_productor)THEN
	      MessageBox("Atención","Debe Seleccionar un Productor Previamente",Exclamation!)
	      RETURN
	   END IF
   END IF
END IF

//Tipo Informe
IF Istr_Mant.Argumento[1] = '0' THEN
	MessageBox("Atención","Debe Seleccionar un Tipo de Informe Previamente",Exclamation!)
	RETURN
END IF

IF cbx_todosfecha.Checked THEN
  ld_FechaEmbaini =	Date(01/01/2000)
  ld_FechaEmbafin =	Today()
  em_fechadesde.text = String(ld_FechaEmbaini,"dd/mm/yyyy")
  em_fechahasta.Text = String(ld_FechaEmbafin,"dd/mm/yyyy")
ELSE
 ld_FechaEmbaini = Date(em_fechadesde.Text)
 ld_FechaEmbafin = Date(em_fechahasta.Text)
END IF

istr_info.titulo	= 'INFORME EVOLUCION DE NIVEL DE MUESTREO'

OpenWithParm(vinf,istr_info)

IF istr_mant.argumento[1] = '1' THEN
	IF ld_FechaEmbafin >= RelativeDate(ld_FechaEmbaini, 30) OR cbx_todosfecha.Checked THEN
	  IF MessageBox ("Atención","El informe solicitado tiene más de 31 días de datos,~r" + & 
						  "~n~n¿Desea hacer una selección semanal? ", Question!, YesNo!,1)	= 1 THEN
		  istr_mant.argumento[1] = '7'
		  istr_mant.argumento[2] = 'Semanal'
		  cbx_dia.Checked    = False
		  cbx_semana.Checked = True
		RETURN 0
	  END IF
	END IF
END IF
vinf.dw_1.DataObject = "dw_info_evolu_muestreo_enca"
	
vinf.dw_1.SetTransObject(sqlca)	
li_fila = vinf.dw_1.Retrieve(Integer(istr_mant.argumento[1]),li_zona,li_especie,&
									 ll_productor,li_planta,ld_FechaEmbaini,ld_FechaEmbafin)
						  
IF li_fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ELSEIF li_fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", &
					StopSign!, Ok!)
ELSE
	F_Membrete(vinf.dw_1)
	vinf.dw_1.Modify("cod_espe.text = '" + String(li_especie) + "'")
	vinf.dw_1.Modify("especie.text = '" + iuo_Especie.nombre + "'")
	vinf.dw_1.Modify("zona_cod.text = '" + String(li_zona) + "'")
   	vinf.dw_1.Modify("zona.text = '" + uo_muestrazona.nombre + "'")
   	vinf.dw_1.Modify("desde.text = '" + em_fechadesde.text + "'")
	vinf.dw_1.Modify("hasta.text = '" + em_fechahasta.text + "'")	
	vinf.dw_1.Modify("tipo.text = '" + Istr_Mant.Argumento[2] + "'")	
	IF gs_Ambiente <> 'Windows' THEN
		F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)

	END IF
END IF

SetPointer(Arrow!)
end event

type pb_salir from w_para_informes`pb_salir within w_info_planillaevolumuestreo
string tag = "Salir [Cerrar Ventana Activa]"
integer x = 2213
integer y = 796
integer taborder = 200
string powertiptext = "Salir [Cerrar Ventana Activa]"
end type

type em_fechadesde from editmask within w_info_planillaevolumuestreo
integer x = 599
integer y = 1260
integer width = 338
integer height = 92
integer taborder = 110
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long backcolor = 33543637
boolean enabled = false
string text = "none"
alignment alignment = center!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
end type

type st_12 from statictext within w_info_planillaevolumuestreo
integer x = 375
integer y = 1272
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
boolean enabled = false
string text = "Desde"
boolean focusrectangle = false
end type

type st_13 from statictext within w_info_planillaevolumuestreo
integer x = 1125
integer y = 1272
integer width = 178
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
boolean enabled = false
string text = "Hasta"
boolean focusrectangle = false
end type

type em_fechahasta from editmask within w_info_planillaevolumuestreo
integer x = 1317
integer y = 1260
integer width = 338
integer height = 92
integer taborder = 120
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long backcolor = 33543637
boolean enabled = false
string text = "none"
alignment alignment = center!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
end type

type st_zona from statictext within w_info_planillaevolumuestreo
integer x = 375
integer y = 708
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
string text = "Zona Origen"
boolean focusrectangle = false
end type

type st_33 from statictext within w_info_planillaevolumuestreo
integer x = 375
integer y = 952
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

type cbx_todosfecha from checkbox within w_info_planillaevolumuestreo
integer x = 1669
integer y = 1272
integer width = 123
integer height = 64
integer taborder = 100
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
long backcolor = 553648127
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

type st_14 from statictext within w_info_planillaevolumuestreo
integer x = 375
integer y = 836
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
string text = "Especie"
boolean focusrectangle = false
end type

type st_2 from statictext within w_info_planillaevolumuestreo
integer x = 1595
integer y = 636
integer width = 197
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
boolean enabled = false
string text = "Todos"
boolean focusrectangle = false
end type

type uo_muestrazona from uo_seleccion_zonas_mod within w_info_planillaevolumuestreo
integer x = 754
integer y = 692
integer width = 1262
integer height = 108
integer taborder = 130
boolean bringtotop = true
end type

on uo_muestrazona.destroy
call uo_seleccion_zonas_mod::destroy
end on

event ue_cambio;call super::ue_cambio;IF IsNull(This.Codigo) THEN RETURN

CHOOSE CASE This.Codigo
	CASE -1, -9
		uo_muestraplanta.Todos(True)
		uo_muestraplanta.cbx_Todos.Enabled	=	False
		
	CASE ELSE
		uo_muestraplanta.Filtra(This.Codigo)
		uo_muestraplanta.cbx_Todos.Enabled	=	True
END CHOOSE
end event

type uo_muestraespecies from uo_seleccion_especie_mod within w_info_planillaevolumuestreo
event destroy ( )
integer x = 754
integer y = 804
integer width = 901
integer height = 112
integer taborder = 150
boolean bringtotop = true
end type

on uo_muestraespecies.destroy
call uo_seleccion_especie_mod::destroy
end on

type st_1 from statictext within w_info_planillaevolumuestreo
integer x = 375
integer y = 1072
integer width = 370
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
string text = "Packing"
boolean focusrectangle = false
end type

type st_4 from statictext within w_info_planillaevolumuestreo
integer x = 302
integer y = 1188
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
string text = "Fecha Embalaje"
boolean focusrectangle = false
end type

type st_8 from statictext within w_info_planillaevolumuestreo
integer x = 1847
integer y = 636
integer width = 201
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
string text = "Cons."
boolean focusrectangle = false
end type

type gb_3 from groupbox within w_info_planillaevolumuestreo
integer x = 302
integer y = 1196
integer width = 1769
integer height = 196
integer taborder = 150
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long backcolor = 553648127
end type

type uo_muestraproductor from uo_seleccion_productor_mod within w_info_planillaevolumuestreo
event destroy ( )
integer x = 754
integer y = 928
integer width = 1262
integer height = 108
integer taborder = 140
boolean bringtotop = true
end type

on uo_muestraproductor.destroy
call uo_seleccion_productor_mod::destroy
end on

type gb_5 from groupbox within w_info_planillaevolumuestreo
integer x = 302
integer y = 420
integer width = 1769
integer height = 196
integer taborder = 40
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Informe"
end type

type st_44 from statictext within w_info_planillaevolumuestreo
integer x = 270
integer y = 412
integer width = 1833
integer height = 1032
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

type ddlb_informe from dropdownlistbox within w_info_planillaevolumuestreo
boolean visible = false
integer x = 279
integer y = 1452
integer width = 859
integer height = 400
integer taborder = 50
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
boolean sorted = false
string item[] = {"Diario","Semana","Quincena"}
borderstyle borderstyle = stylelowered!
end type

event selectionchanged;IF Integer(index) = 1 THEN
	istr_Mant.Argumento[1] = '1'
ELSEIF Integer(index) = 2 THEN
	istr_Mant.Argumento[1] = '7'
ELSEIF Integer(index) = 3 THEN
	istr_Mant.Argumento[1] = '15'
END IF


end event

type uo_muestraplanta from uo_seleccion_frigopacking_mod within w_info_planillaevolumuestreo
integer x = 750
integer y = 1048
integer height = 128
integer taborder = 150
boolean bringtotop = true
end type

on uo_muestraplanta.destroy
call uo_seleccion_frigopacking_mod::destroy
end on

type cbx_dia from checkbox within w_info_planillaevolumuestreo
integer x = 576
integer y = 488
integer width = 274
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
string text = "Diario"
boolean lefttext = true
end type

event clicked;IF This.Checked THEN
	Istr_Mant.Argumento[1] = '1'
	Istr_Mant.Argumento[2] = This.Text
	cbx_semana.Checked = False
	cbx_quincena.Checked = False
ELSE
	Istr_Mant.Argumento[1] = '0'
	Istr_Mant.Argumento[2] = ''
END IF
end event

type cbx_semana from checkbox within w_info_planillaevolumuestreo
integer x = 1033
integer y = 488
integer width = 343
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
string text = "Semanal"
boolean checked = true
boolean lefttext = true
end type

event clicked;IF This.Checked THEN
	Istr_Mant.Argumento[1] = '7'
	Istr_Mant.Argumento[2] = This.Text
	cbx_dia.Checked = False
	cbx_quincena.Checked = False
ELSE
	Istr_Mant.Argumento[1] = '0'
	Istr_Mant.Argumento[2] = ''
END IF
	
end event

type cbx_quincena from checkbox within w_info_planillaevolumuestreo
integer x = 1554
integer y = 488
integer width = 379
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
string text = "Quincena"
boolean lefttext = true
end type

event clicked;IF This.Checked THEN
	Istr_Mant.Argumento[1] = '15'
	Istr_Mant.Argumento[2] = This.Text
	cbx_dia.Checked = False
	cbx_semana.Checked = False
ELSE
	Istr_Mant.Argumento[1] = '0'
	Istr_Mant.Argumento[2] = ''
END IF
end event

