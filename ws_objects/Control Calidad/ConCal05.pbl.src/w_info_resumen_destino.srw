$PBExportHeader$w_info_resumen_destino.srw
$PBExportComments$Ventana de Informe de Resumen Objeciones
forward
global type w_info_resumen_destino from w_para_informes
end type
type em_fechadesde from editmask within w_info_resumen_destino
end type
type st_12 from statictext within w_info_resumen_destino
end type
type st_13 from statictext within w_info_resumen_destino
end type
type em_fechahasta from editmask within w_info_resumen_destino
end type
type st_zona from statictext within w_info_resumen_destino
end type
type st_33 from statictext within w_info_resumen_destino
end type
type cbx_todosfecha from checkbox within w_info_resumen_destino
end type
type st_9 from statictext within w_info_resumen_destino
end type
type st_10 from statictext within w_info_resumen_destino
end type
type cbx_nave from checkbox within w_info_resumen_destino
end type
type st_11 from statictext within w_info_resumen_destino
end type
type dw_nave from datawindow within w_info_resumen_destino
end type
type st_14 from statictext within w_info_resumen_destino
end type
type st_3 from statictext within w_info_resumen_destino
end type
type st_5 from statictext within w_info_resumen_destino
end type
type ddlb_mercado from dropdownlistbox within w_info_resumen_destino
end type
type cbx_mercado from checkbox within w_info_resumen_destino
end type
type st_44 from statictext within w_info_resumen_destino
end type
type cbx_consmercado from checkbox within w_info_resumen_destino
end type
type cbx_consnave from checkbox within w_info_resumen_destino
end type
type cbx_consfecha from checkbox within w_info_resumen_destino
end type
type uo_tipotrans from uo_seleccion_tipotransporte_mod within w_info_resumen_destino
end type
type st_2 from statictext within w_info_resumen_destino
end type
type st_8 from statictext within w_info_resumen_destino
end type
type uo_muestrarecibidor from uo_seleccion_recibidor_planilla_mod within w_info_resumen_destino
end type
type uo_muestrazona from uo_seleccion_zonas_mod within w_info_resumen_destino
end type
type uo_muestraproductor from uo_seleccion_productor_zonas_mod within w_info_resumen_destino
end type
type uo_muestraespecies from uo_seleccion_especie_mod within w_info_resumen_destino
end type
type uo_muestravariedad from uo_seleccion_variedad_mod within w_info_resumen_destino
end type
type st_15 from statictext within w_info_resumen_destino
end type
type st_16 from statictext within w_info_resumen_destino
end type
type gb_4 from groupbox within w_info_resumen_destino
end type
type st_7 from statictext within w_info_resumen_destino
end type
type uo_selembalaje from uo_seleccion_embalaje_mod within w_info_resumen_destino
end type
type uo_seleccalibre from uo_seleccion_calibre_mod within w_info_resumen_destino
end type
type st_1 from statictext within w_info_resumen_destino
end type
type st_6 from statictext within w_info_resumen_destino
end type
type gb_3 from groupbox within w_info_resumen_destino
end type
type st_4 from statictext within w_info_resumen_destino
end type
end forward

global type w_info_resumen_destino from w_para_informes
integer x = 14
integer y = 32
integer width = 3753
integer height = 1724
string title = "Resumen Gráfico de Lotes en Destino"
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
st_3 st_3
st_5 st_5
ddlb_mercado ddlb_mercado
cbx_mercado cbx_mercado
st_44 st_44
cbx_consmercado cbx_consmercado
cbx_consnave cbx_consnave
cbx_consfecha cbx_consfecha
uo_tipotrans uo_tipotrans
st_2 st_2
st_8 st_8
uo_muestrarecibidor uo_muestrarecibidor
uo_muestrazona uo_muestrazona
uo_muestraproductor uo_muestraproductor
uo_muestraespecies uo_muestraespecies
uo_muestravariedad uo_muestravariedad
st_15 st_15
st_16 st_16
gb_4 gb_4
st_7 st_7
uo_selembalaje uo_selembalaje
uo_seleccalibre uo_seleccalibre
st_1 st_1
st_6 st_6
gb_3 gb_3
st_4 st_4
end type
global w_info_resumen_destino w_info_resumen_destino

type variables
str_busqueda istr_busq
str_mant istr_mant
Integer	ii_tipo
String	is_report, is_nula

DataWindowChild		idwc_nave, idwc_planilla
uo_naves             iuo_naves
uo_nroplanilla       iuo_nroplanilla




end variables

on w_info_resumen_destino.create
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
this.st_3=create st_3
this.st_5=create st_5
this.ddlb_mercado=create ddlb_mercado
this.cbx_mercado=create cbx_mercado
this.st_44=create st_44
this.cbx_consmercado=create cbx_consmercado
this.cbx_consnave=create cbx_consnave
this.cbx_consfecha=create cbx_consfecha
this.uo_tipotrans=create uo_tipotrans
this.st_2=create st_2
this.st_8=create st_8
this.uo_muestrarecibidor=create uo_muestrarecibidor
this.uo_muestrazona=create uo_muestrazona
this.uo_muestraproductor=create uo_muestraproductor
this.uo_muestraespecies=create uo_muestraespecies
this.uo_muestravariedad=create uo_muestravariedad
this.st_15=create st_15
this.st_16=create st_16
this.gb_4=create gb_4
this.st_7=create st_7
this.uo_selembalaje=create uo_selembalaje
this.uo_seleccalibre=create uo_seleccalibre
this.st_1=create st_1
this.st_6=create st_6
this.gb_3=create gb_3
this.st_4=create st_4
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
this.Control[iCurrent+14]=this.st_3
this.Control[iCurrent+15]=this.st_5
this.Control[iCurrent+16]=this.ddlb_mercado
this.Control[iCurrent+17]=this.cbx_mercado
this.Control[iCurrent+18]=this.st_44
this.Control[iCurrent+19]=this.cbx_consmercado
this.Control[iCurrent+20]=this.cbx_consnave
this.Control[iCurrent+21]=this.cbx_consfecha
this.Control[iCurrent+22]=this.uo_tipotrans
this.Control[iCurrent+23]=this.st_2
this.Control[iCurrent+24]=this.st_8
this.Control[iCurrent+25]=this.uo_muestrarecibidor
this.Control[iCurrent+26]=this.uo_muestrazona
this.Control[iCurrent+27]=this.uo_muestraproductor
this.Control[iCurrent+28]=this.uo_muestraespecies
this.Control[iCurrent+29]=this.uo_muestravariedad
this.Control[iCurrent+30]=this.st_15
this.Control[iCurrent+31]=this.st_16
this.Control[iCurrent+32]=this.gb_4
this.Control[iCurrent+33]=this.st_7
this.Control[iCurrent+34]=this.uo_selembalaje
this.Control[iCurrent+35]=this.uo_seleccalibre
this.Control[iCurrent+36]=this.st_1
this.Control[iCurrent+37]=this.st_6
this.Control[iCurrent+38]=this.gb_3
this.Control[iCurrent+39]=this.st_4
end on

on w_info_resumen_destino.destroy
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
destroy(this.st_3)
destroy(this.st_5)
destroy(this.ddlb_mercado)
destroy(this.cbx_mercado)
destroy(this.st_44)
destroy(this.cbx_consmercado)
destroy(this.cbx_consnave)
destroy(this.cbx_consfecha)
destroy(this.uo_tipotrans)
destroy(this.st_2)
destroy(this.st_8)
destroy(this.uo_muestrarecibidor)
destroy(this.uo_muestrazona)
destroy(this.uo_muestraproductor)
destroy(this.uo_muestraespecies)
destroy(this.uo_muestravariedad)
destroy(this.st_15)
destroy(this.st_16)
destroy(this.gb_4)
destroy(this.st_7)
destroy(this.uo_selembalaje)
destroy(this.uo_seleccalibre)
destroy(this.st_1)
destroy(this.st_6)
destroy(this.gb_3)
destroy(this.st_4)
end on

event open;call super::open;
x = 0
y = 0
cbx_mercado.SetFocus()
This.Icon	=	Gstr_apl.Icono

iuo_naves             =  Create uo_naves
iuo_nroplanilla       =  Create uo_nroplanilla

//istr_mant.argumento[1] = codigo de mercado
Boolean	lb_Cerrar

IF IsNull(uo_tipotrans.Codigo) THEN lb_Cerrar	=	True

//IF IsNull(uo_muestrarecibidor.Codigo) THEN lb_Cerrar	=	True

IF IsNull(uo_muestraespecies.Codigo) THEN lb_Cerrar	=	True

IF IsNull(uo_muestravariedad.Codigo) THEN lb_Cerrar	=	True

IF IsNull(uo_muestrazona.Codigo) THEN lb_Cerrar	=	True

IF IsNull(uo_muestraproductor.Codigo) THEN lb_Cerrar	=	True

IF IsNull(uo_selembalaje.Codigo) THEN lb_Cerrar	=	True

IF isnull(uo_seleccalibre.codigo) THEN lb_Cerrar = True

IF lb_Cerrar THEN
	Close(This)
ELSE
	uo_tipotrans.Seleccion(True, True)
	uo_muestrarecibidor.Seleccion(True, True)
	uo_muestraespecies.Seleccion(True, True)
	uo_muestravariedad.Seleccion(True, True)
	uo_muestrazona.Seleccion(True, True)
	uo_muestraproductor.Seleccion(True, True)
	uo_selembalaje.Seleccion(True, True)
	uo_seleccalibre.Seleccion(True, True)


//ddlb_mercado.enabled    = FALSE
//cbx_consmercado.checked = FALSE
//Nave							
dw_nave.GetChild("nave_codigo", idwc_nave)
idwc_nave.SetTransObject(sqlca)
idwc_nave.Retrieve(0, '*')
dw_nave.InsertRow(0)
idwc_nave.SetSort("nave_nombre A")
idwc_nave.Sort()
//dw_nave.enabled = FALSE
//cbx_consnave.checked = FALSE


//TipoTransporte
//uo_tipotrans.cbx_consolida.checked  = FALSE
uo_tipotrans.cbx_consolida.visible  = FALSE
uo_tipotrans.cbx_todos.checked      = FALSE
uo_tipotrans.cbx_todos.visible      = FALSE
uo_tipotrans.dw_Seleccion.Enabled	= TRUE
uo_tipotrans.dw_seleccion.object.codigo[1] = 'M'

//Recibidor
uo_muestrarecibidor.cbx_consolida.checked = FALSE
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
uo_muestravariedad.filtra(11)

//Zona
//uo_muestrazona.cbx_consolida.checked = FALSE

//Productor
uo_muestraproductor.cbx_consolida.checked = FALSE
uo_muestraproductor.filtra(0)

//Embalaje
uo_selembalaje.cbx_consolida.checked = FALSE
uo_selembalaje.filtra('U')
//calibre
uo_seleccalibre.Filtra(81, 11, 0)

//uo_muestrarecibidor.dw_Seleccion.Object.codigo.BackGround.Color	=	RGB(255, 255, 255)
//uo_muestraespecies.dw_Seleccion.Object.codigo.BackGround.Color	=	RGB(255, 255, 255)	
//uo_muestravariedad.dw_Seleccion.Object.codigo.BackGround.Color	=	RGB(255, 255, 255)
//uo_muestrazona.dw_Seleccion.Object.codigo.BackGround.Color	=	RGB(255, 255, 255)	
//uo_muestraproductor.dw_Seleccion.Object.codigo.BackGround.Color	=	RGB(255, 255, 255)	
//uo_tipotrans.dw_Seleccion.Object.codigo.BackGround.Color	=	RGB(255, 255, 255)	
//uo_selembalaje.dw_Seleccion.Object.codigo.BackGround.Color	=	RGB(255, 255, 255)
//dw_nave.Object.nave_codigo.BackGround.Color	=	RGB(255, 255, 255)	
//ddlb_mercado.backcolor = RGB(255, 255, 255)

istr_mant.argumento[1] = ''
em_fechadesde.text	  = String(Today())
em_fechahasta.text	  = String(Today())
cbx_consfecha.checked  = FALSE

END IF

end event

type st_computador from w_para_informes`st_computador within w_info_resumen_destino
integer x = 2800
integer y = 152
end type

type st_usuario from w_para_informes`st_usuario within w_info_resumen_destino
integer x = 2800
integer y = 80
end type

type st_temporada from w_para_informes`st_temporada within w_info_resumen_destino
integer x = 2800
integer y = 8
end type

type p_logo from w_para_informes`p_logo within w_info_resumen_destino
end type

type st_titulo from w_para_informes`st_titulo within w_info_resumen_destino
integer width = 3145
string text = "Resumen Gráfico de Lotes en Destino"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_resumen_destino
string tag = "Imprimir Reporte"
integer x = 3456
integer y = 460
integer taborder = 190
string powertiptext = "Imprimir Reporte"
end type

event pb_acepta::clicked;SetPointer(Arrow!)
Integer	li_fila, li_mercado, li_nave, li_recibidor, li_zona, li_especie, li_variedad, li_consfec  
Date		ld_FechaEmbaini, ld_FechaEmbafin
String   ls_embalaje, ls_tipotrans,ls_calibre
Long     ll_Productor, ll_planilla

SetPointer(HourGlass!)

//Mercado
IF cbx_consmercado.Checked THEN
	li_mercado  = -9
ELSE
   IF cbx_mercado.Checked THEN
      li_mercado	= -1
   ELSE
      li_mercado	= Integer(istr_mant.argumento[1])
	   IF IsNull(li_mercado) OR li_mercado=0 THEN
	      MessageBox("Atención","Debe Seleccionar un Mercado Previamente",Exclamation!)
	      RETURN
	   END IF
   END IF
END IF

//Tiponave

ls_tipotrans	= uo_tipotrans.dw_Seleccion.Object.codigo[1]
IF IsNull(ls_tipotrans)THEN
   MessageBox("Atención","Debe Seleccionar un Tipo de Transporte Previamente",Exclamation!)
   RETURN
END IF

//Nave
IF cbx_consnave.Checked THEN
	li_nave  = -9
ELSE
   IF cbx_nave.Checked THEN
      li_nave	= -1
   ELSE
      li_nave	= dw_nave.Object.nave_codigo[1]
	   IF IsNull(li_nave)THEN
	      MessageBox("Atención","Debe Seleccionar una Nave Previamente",Exclamation!)
	   RETURN
	   END IF
   END IF
END IF


//Recibidor
IF uo_muestrarecibidor.cbx_consolida.checked THEN
	li_recibidor   = -9
ELSE
   IF uo_muestrarecibidor.cbx_todos.checked THEN
      li_recibidor	= -1
   ELSE
      li_recibidor	= uo_muestrarecibidor.dw_Seleccion.Object.codigo[1]
	   IF IsNull(li_recibidor)THEN
	      MessageBox("Atención","Debe Seleccionar un Recibidor Previamente",Exclamation!)
	      RETURN
	   END IF
   END IF
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


li_especie	= uo_muestraespecies.dw_Seleccion.Object.codigo[1]
IF IsNull(li_especie)THEN
   MessageBox("Atención","Debe Seleccionar una Especie Previamente",Exclamation!)
	RETURN
END IF


//Variedad
IF uo_muestravariedad.cbx_consolida.checked THEN
	li_variedad    = -9
ELSE
   IF uo_muestravariedad.cbx_todos.checked THEN
	   li_variedad 	= -1
   ELSE
      li_variedad	= uo_muestravariedad.dw_Seleccion.Object.codigo[1]
	   IF IsNull(li_variedad)THEN
	      MessageBox("Atención","Debe Seleccionar una Variedad Previamente",Exclamation!)
	      RETURN
	   END IF
   END IF
END IF

//Embalaje
IF uo_selembalaje.cbx_consolida.checked THEN
   ls_embalaje  = '**'
ELSE
	IF uo_selembalaje.cbx_todos.checked THEN
      ls_embalaje	= '*'
   ELSE
      ls_embalaje	= uo_selembalaje.dw_Seleccion.Object.codigo[1]
	   IF IsNull(ls_embalaje)THEN
	      MessageBox("Atención","Debe Seleccionar Emabalaje Previamente",Exclamation!)
	      RETURN
	   END IF
   END IF
END IF

IF uo_seleccalibre.cbx_consolida.checked THEN
   ls_calibre  = '**'
ELSE
	IF uo_seleccalibre.cbx_todos.checked THEN
      ls_calibre	= '*'
   ELSE
      ls_calibre	= uo_seleccalibre.dw_Seleccion.Object.codigo[1]
	   IF IsNull(ls_calibre)THEN
	      MessageBox("Atención","Debe Seleccionar Calibre Previamente",Exclamation!)
	      RETURN
	   END IF
   END IF
END IF

//Fecha Embalaje
IF cbx_consfecha.Checked THEN
	li_consfec  = -9
	ld_FechaEmbaini =	Date(01/01/2000)
	ld_FechaEmbafin =	Today()
ELSE
  IF cbx_todosfecha.Checked THEN
	  li_consfec = -1
	  ld_FechaEmbaini =	Date(01/01/2000)
	  ld_FechaEmbafin =	Today()
	  em_fechadesde.text = String(ld_FechaEmbaini,"dd/mm/yyyy")
	  em_fechahasta.Text = String(ld_FechaEmbafin,"dd/mm/yyyy")
  ELSE
    ld_FechaEmbaini = Date(em_fechadesde.Text)
	 ld_FechaEmbafin = Date(em_fechahasta.Text)
  END IF
END IF


istr_info.titulo	= 'RESUMEN GRÁFICO DE LOTES EN DESTINO'

OpenWithParm(vinf,istr_info)

vinf.dw_1.DataObject = "dw_info_resumen_inspecciondestino"


vinf.dw_1.SetTransObject(sqlca)

li_fila = vinf.dw_1.Retrieve(gi_CodExport,li_mercado,ls_tipotrans,li_nave,li_recibidor,li_especie,&
li_variedad,li_zona,ll_productor,ls_embalaje,ld_FechaEmbaini,ld_FechaEmbafin,li_consfec,ls_calibre )


IF li_fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ELSEIF li_fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", &
					StopSign!, Ok!)
ELSE
	F_Membrete(vinf.dw_1)
	vinf.dw_1.Modify("desde.text = '" + em_fechadesde.text + "'")
	vinf.dw_1.Modify("hasta.text = '" + em_fechahasta.text + "'")	
	vinf.dw_1.Object.DataWindow.Zoom = 78
	IF gs_Ambiente <> 'Windows' THEN
		F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)

	END IF
END IF

SetPointer(Arrow!)

end event

type pb_salir from w_para_informes`pb_salir within w_info_resumen_destino
string tag = "Salir [Cerrar Ventana Activa]"
integer x = 3456
integer y = 740
integer taborder = 200
string powertiptext = "Salir [Cerrar Ventana Activa]"
end type

type em_fechadesde from editmask within w_info_resumen_destino
integer x = 443
integer y = 1236
integer width = 370
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
alignment alignment = center!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
end type

type st_12 from statictext within w_info_resumen_destino
integer x = 453
integer y = 1172
integer width = 201
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

type st_13 from statictext within w_info_resumen_destino
integer x = 1097
integer y = 1172
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

type em_fechahasta from editmask within w_info_resumen_destino
integer x = 1079
integer y = 1236
integer width = 370
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
alignment alignment = center!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
end type

type st_zona from statictext within w_info_resumen_destino
integer x = 1824
integer y = 552
integer width = 320
integer height = 84
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
boolean enabled = false
string text = "Zona"
boolean focusrectangle = false
end type

type st_33 from statictext within w_info_resumen_destino
integer x = 1824
integer y = 692
integer width = 329
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

type cbx_todosfecha from checkbox within w_info_resumen_destino
integer x = 1513
integer y = 1248
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

type st_9 from statictext within w_info_resumen_destino
integer x = 311
integer y = 544
integer width = 251
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

type st_10 from statictext within w_info_resumen_destino
integer x = 311
integer y = 820
integer width = 192
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

type cbx_nave from checkbox within w_info_resumen_destino
integer x = 1527
integer y = 824
integer width = 123
integer height = 64
integer taborder = 60
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
	//cbx_consnave.enabled = TRUE
	cbx_consnave.checked = FALSE
ELSE
	//dw_nave.Enabled	=	TRUE
	dw_nave.Setfocus()
	//dw_nave.Object.nave_codigo.BackGround.Color	=	RGB(255, 255, 255)
	
	//cbx_consnave.enabled = FALSE
END IF
end event

type st_11 from statictext within w_info_resumen_destino
integer x = 311
integer y = 948
integer width = 288
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

type dw_nave from datawindow within w_info_resumen_destino
integer x = 608
integer y = 812
integer width = 896
integer height = 84
integer taborder = 70
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
cbx_consnave.Checked = False
end event

type st_14 from statictext within w_info_resumen_destino
integer x = 1824
integer y = 820
integer width = 329
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

type st_3 from statictext within w_info_resumen_destino
integer x = 1824
integer y = 952
integer width = 329
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

type st_5 from statictext within w_info_resumen_destino
integer x = 311
integer y = 684
integer width = 306
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
string text = "Tip. Nave"
boolean focusrectangle = false
end type

type ddlb_mercado from dropdownlistbox within w_info_resumen_destino
integer x = 608
integer y = 548
integer width = 896
integer height = 252
integer taborder = 30
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
cbx_consmercado.Checked = False
end event

type cbx_mercado from checkbox within w_info_resumen_destino
integer x = 1527
integer y = 556
integer width = 123
integer height = 64
integer taborder = 20
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
	/*recibidor*/
   uo_muestrarecibidor.filtra(0)
	//ddlb_mercado.backcolor = RGB(192, 192, 192)
	//ddlb_mercado.enabled = FALSE
	//cbx_consmercado.enabled = TRUE
	cbx_consmercado.checked = FALSE
	ddlb_mercado.SelectItem ( 0 )
ELSE
	//ddlb_mercado.enabled = TRUE
	//ddlb_mercado.backcolor = RGB(255, 255, 255)
	ddlb_mercado.SetFocus()
	//cbx_consmercado.enabled = FALSE
END IF
return 0

end event

type st_44 from statictext within w_info_resumen_destino
integer x = 247
integer y = 396
integer width = 3145
integer height = 680
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

type cbx_consmercado from checkbox within w_info_resumen_destino
integer x = 1723
integer y = 556
integer width = 101
integer height = 64
integer taborder = 10
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
string text = " "
end type

event clicked;Integer	li_null

SetNull(li_null)
IF This.Checked THEN
	/*Nave*/
	idwc_nave.Retrieve(0,'*')
	/*recibidor*/
   uo_muestrarecibidor.filtra(0)
	//ddlb_mercado.backcolor = RGB(192, 192, 192)
	//ddlb_mercado.enabled = FALSE
	//cbx_consmercado.enabled = TRUE
	cbx_mercado.checked = FALSE
	ddlb_mercado.SelectItem ( 0 )
ELSE
	//ddlb_mercado.enabled = TRUE
	//ddlb_mercado.backcolor = RGB(255, 255, 255)
	ddlb_mercado.SetFocus()
	//cbx_consmercado.enabled = FALSE
END IF
return 0

end event

type cbx_consnave from checkbox within w_info_resumen_destino
integer x = 1723
integer y = 824
integer width = 105
integer height = 64
integer taborder = 50
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
string text = " "
end type

event clicked;Integer	li_Null

SetNull(li_Null)
IF This.Checked THEN
	//dw_nave.Enabled	=	FALSE	
	dw_nave.SetItem(1, "nave_codigo", li_Null)
	//dw_nave.Object.nave_codigo.BackGround.Color	=	RGB(192, 192, 192)
	//cbx_consnave.enabled = TRUE
	cbx_nave.checked = FALSE
ELSE
	//dw_nave.Enabled	=	TRUE
	dw_nave.Setfocus()
	//dw_nave.Object.nave_codigo.BackGround.Color	=	RGB(255, 255, 255)
	
	//cbx_consnave.enabled = FALSE
END IF
end event

type cbx_consfecha from checkbox within w_info_resumen_destino
integer x = 1710
integer y = 1248
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

type uo_tipotrans from uo_seleccion_tipotransporte_mod within w_info_resumen_destino
integer x = 608
integer y = 664
integer width = 1216
integer taborder = 40
boolean bringtotop = true
end type

on uo_tipotrans.destroy
call uo_seleccion_tipotransporte_mod::destroy
end on

type st_2 from statictext within w_info_resumen_destino
integer x = 1472
integer y = 452
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

type st_8 from statictext within w_info_resumen_destino
integer x = 1673
integer y = 452
integer width = 206
integer height = 60
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
boolean enabled = false
string text = "Cons."
boolean focusrectangle = false
end type

type uo_muestrarecibidor from uo_seleccion_recibidor_planilla_mod within w_info_resumen_destino
event destroy ( )
integer x = 608
integer y = 924
integer width = 1216
integer taborder = 80
boolean bringtotop = true
end type

on uo_muestrarecibidor.destroy
call uo_seleccion_recibidor_planilla_mod::destroy
end on

type uo_muestrazona from uo_seleccion_zonas_mod within w_info_resumen_destino
integer x = 2139
integer y = 524
integer width = 1234
integer taborder = 130
boolean bringtotop = true
end type

on uo_muestrazona.destroy
call uo_seleccion_zonas_mod::destroy
end on

event ue_cambio;call super::ue_cambio;IF IsNull(This.Codigo) THEN RETURN

CHOOSE CASE This.Codigo
	CASE -1, -9
		uo_muestraproductor.Todos(True)
		
		//uo_muestraproductor.cbx_Todos.Enabled	=	False
		
	CASE ELSE
		uo_muestraproductor.Filtra(This.Codigo)
		
		//uo_muestraproductor.cbx_Todos.Enabled	=	True
END CHOOSE
end event

type uo_muestraproductor from uo_seleccion_productor_zonas_mod within w_info_resumen_destino
integer x = 2139
integer y = 664
integer width = 1234
integer taborder = 140
boolean bringtotop = true
end type

on uo_muestraproductor.destroy
call uo_seleccion_productor_zonas_mod::destroy
end on

type uo_muestraespecies from uo_seleccion_especie_mod within w_info_resumen_destino
event destroy ( )
integer x = 2139
integer y = 792
integer width = 1234
integer height = 128
integer taborder = 150
boolean bringtotop = true
end type

on uo_muestraespecies.destroy
call uo_seleccion_especie_mod::destroy
end on

event ue_cambio;call super::ue_cambio;String ls_especie

IF IsNull(This.Codigo) THEN RETURN

CHOOSE CASE This.Codigo
	CASE -1, -9
		uo_muestravariedad.Todos(True)
		uo_selembalaje.Todos(True)
		
		uo_muestravariedad.cbx_Todos.Enabled	=	False
		uo_selembalaje.cbx_Todos.Enabled	=	False
		
	CASE ELSE
		uo_muestravariedad.Filtra(This.Codigo)
		
		
		//RECUPERA NOMBRE DE ESPECIE PARA PODER FILTRAR EMBALAJE
		
  SELECT espe_nombre  
    INTO :ls_especie  
    FROM dba.especies  
   WHERE ( espe_codigo =: This.Codigo)   ;

	
		uo_selembalaje.Filtra(MID(ls_especie, 1, 1))
		
		uo_muestravariedad.cbx_Todos.Enabled	=	True
		
END CHOOSE
end event

type uo_muestravariedad from uo_seleccion_variedad_mod within w_info_resumen_destino
event destroy ( )
integer x = 2139
integer y = 928
integer width = 1234
integer taborder = 160
boolean bringtotop = true
end type

on uo_muestravariedad.destroy
call uo_seleccion_variedad_mod::destroy
end on

event ue_cambio;call super::ue_cambio;IF IsNull(This.Codigo) THEN RETURN

CHOOSE CASE This.Codigo
	CASE -1, -9
		uo_seleccalibre.Todos(True)
		uo_seleccalibre.Filtra(81,uo_muestraespecies.dw_seleccion.object.codigo[1], 0)		
	CASE ELSE
		uo_seleccalibre.Filtra(gi_CodExport,uo_muestraespecies.dw_seleccion.object.codigo[1],This.Codigo)
END CHOOSE
end event

type st_15 from statictext within w_info_resumen_destino
integer x = 2990
integer y = 452
integer width = 201
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

type st_16 from statictext within w_info_resumen_destino
integer x = 3200
integer y = 452
integer width = 187
integer height = 60
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
boolean enabled = false
string text = "Cons."
boolean focusrectangle = false
end type

type gb_4 from groupbox within w_info_resumen_destino
integer x = 1856
integer y = 1092
integer width = 1518
integer height = 320
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
end type

type st_7 from statictext within w_info_resumen_destino
integer x = 1838
integer y = 1080
integer width = 1559
integer height = 368
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

type uo_selembalaje from uo_seleccion_embalaje_mod within w_info_resumen_destino
event destroy ( )
integer x = 2126
integer y = 1136
integer width = 1216
integer taborder = 170
boolean bringtotop = true
end type

on uo_selembalaje.destroy
call uo_seleccion_embalaje_mod::destroy
end on

type uo_seleccalibre from uo_seleccion_calibre_mod within w_info_resumen_destino
integer x = 2126
integer y = 1268
integer width = 1216
integer height = 120
integer taborder = 180
boolean bringtotop = true
end type

on uo_seleccalibre.destroy
call uo_seleccion_calibre_mod::destroy
end on

type st_1 from statictext within w_info_resumen_destino
integer x = 1879
integer y = 1168
integer width = 229
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
string text = "Embal."
boolean focusrectangle = false
end type

type st_6 from statictext within w_info_resumen_destino
integer x = 1879
integer y = 1296
integer width = 229
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
string text = "Calibre"
boolean focusrectangle = false
end type

type gb_3 from groupbox within w_info_resumen_destino
integer x = 270
integer y = 1092
integer width = 1541
integer height = 320
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
string text = "Fecha Embalaje"
end type

type st_4 from statictext within w_info_resumen_destino
integer x = 251
integer y = 1080
integer width = 1582
integer height = 368
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

