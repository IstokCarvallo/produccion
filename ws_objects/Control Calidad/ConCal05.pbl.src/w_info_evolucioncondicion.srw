$PBExportHeader$w_info_evolucioncondicion.srw
$PBExportComments$Ventana de Informe de Resumen Objeciones
forward
global type w_info_evolucioncondicion from w_para_informes
end type
type em_fechadesde from editmask within w_info_evolucioncondicion
end type
type st_12 from statictext within w_info_evolucioncondicion
end type
type st_13 from statictext within w_info_evolucioncondicion
end type
type em_fechahasta from editmask within w_info_evolucioncondicion
end type
type st_zona from statictext within w_info_evolucioncondicion
end type
type st_33 from statictext within w_info_evolucioncondicion
end type
type cbx_todosfecha from checkbox within w_info_evolucioncondicion
end type
type st_9 from statictext within w_info_evolucioncondicion
end type
type st_11 from statictext within w_info_evolucioncondicion
end type
type st_14 from statictext within w_info_evolucioncondicion
end type
type st_1 from statictext within w_info_evolucioncondicion
end type
type st_3 from statictext within w_info_evolucioncondicion
end type
type ddlb_mercado from dropdownlistbox within w_info_evolucioncondicion
end type
type cbx_mercado from checkbox within w_info_evolucioncondicion
end type
type st_44 from statictext within w_info_evolucioncondicion
end type
type st_6 from statictext within w_info_evolucioncondicion
end type
type cbx_consmercado from checkbox within w_info_evolucioncondicion
end type
type cbx_consfecha from checkbox within w_info_evolucioncondicion
end type
type uo_muestrarecibidor from uo_seleccion_recibidor_planilla_mod within w_info_evolucioncondicion
end type
type uo_muestraespecies from uo_seleccion_especie_mod within w_info_evolucioncondicion
end type
type uo_muestravariedad from uo_seleccion_variedad_mod within w_info_evolucioncondicion
end type
type uo_muestrazona from uo_seleccion_zonas_mod within w_info_evolucioncondicion
end type
type uo_muestraproductor from uo_seleccion_productor_zonas_mod within w_info_evolucioncondicion
end type
type uo_selembalaje from uo_seleccion_embalaje_mod within w_info_evolucioncondicion
end type
type st_2 from statictext within w_info_evolucioncondicion
end type
type st_4 from statictext within w_info_evolucioncondicion
end type
type st_5 from statictext within w_info_evolucioncondicion
end type
type st_7 from statictext within w_info_evolucioncondicion
end type
end forward

global type w_info_evolucioncondicion from w_para_informes
integer x = 14
integer y = 32
integer width = 3630
integer height = 1076
string title = "Informe de Evolución de la Condición (Origen - Destino )"
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
st_11 st_11
st_14 st_14
st_1 st_1
st_3 st_3
ddlb_mercado ddlb_mercado
cbx_mercado cbx_mercado
st_44 st_44
st_6 st_6
cbx_consmercado cbx_consmercado
cbx_consfecha cbx_consfecha
uo_muestrarecibidor uo_muestrarecibidor
uo_muestraespecies uo_muestraespecies
uo_muestravariedad uo_muestravariedad
uo_muestrazona uo_muestrazona
uo_muestraproductor uo_muestraproductor
uo_selembalaje uo_selembalaje
st_2 st_2
st_4 st_4
st_5 st_5
st_7 st_7
end type
global w_info_evolucioncondicion w_info_evolucioncondicion

type variables
str_busqueda istr_busq
str_mant istr_mant
Integer	ii_tipo
String	is_report, is_nula






end variables

forward prototypes
public function string captura_recibidor (long ai_recibidor)
end prototypes

public function string captura_recibidor (long ai_recibidor); String ls_nombre
 
SELECT reci_nombre 
  INTO :ls_nombre  
  FROM dba.recibidores  
 WHERE reci_codigo = :ai_recibidor ;
		 
	IF sqlca.sqlcode	=	-1	THEN
	   F_ErrorBaseDatos(sqlca,"No se pudo leer la tabla de Recibidores") 
   END IF
	
	
RETURN ls_nombre	

end function

on w_info_evolucioncondicion.create
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
this.st_11=create st_11
this.st_14=create st_14
this.st_1=create st_1
this.st_3=create st_3
this.ddlb_mercado=create ddlb_mercado
this.cbx_mercado=create cbx_mercado
this.st_44=create st_44
this.st_6=create st_6
this.cbx_consmercado=create cbx_consmercado
this.cbx_consfecha=create cbx_consfecha
this.uo_muestrarecibidor=create uo_muestrarecibidor
this.uo_muestraespecies=create uo_muestraespecies
this.uo_muestravariedad=create uo_muestravariedad
this.uo_muestrazona=create uo_muestrazona
this.uo_muestraproductor=create uo_muestraproductor
this.uo_selembalaje=create uo_selembalaje
this.st_2=create st_2
this.st_4=create st_4
this.st_5=create st_5
this.st_7=create st_7
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.em_fechadesde
this.Control[iCurrent+2]=this.st_12
this.Control[iCurrent+3]=this.st_13
this.Control[iCurrent+4]=this.em_fechahasta
this.Control[iCurrent+5]=this.st_zona
this.Control[iCurrent+6]=this.st_33
this.Control[iCurrent+7]=this.cbx_todosfecha
this.Control[iCurrent+8]=this.st_9
this.Control[iCurrent+9]=this.st_11
this.Control[iCurrent+10]=this.st_14
this.Control[iCurrent+11]=this.st_1
this.Control[iCurrent+12]=this.st_3
this.Control[iCurrent+13]=this.ddlb_mercado
this.Control[iCurrent+14]=this.cbx_mercado
this.Control[iCurrent+15]=this.st_44
this.Control[iCurrent+16]=this.st_6
this.Control[iCurrent+17]=this.cbx_consmercado
this.Control[iCurrent+18]=this.cbx_consfecha
this.Control[iCurrent+19]=this.uo_muestrarecibidor
this.Control[iCurrent+20]=this.uo_muestraespecies
this.Control[iCurrent+21]=this.uo_muestravariedad
this.Control[iCurrent+22]=this.uo_muestrazona
this.Control[iCurrent+23]=this.uo_muestraproductor
this.Control[iCurrent+24]=this.uo_selembalaje
this.Control[iCurrent+25]=this.st_2
this.Control[iCurrent+26]=this.st_4
this.Control[iCurrent+27]=this.st_5
this.Control[iCurrent+28]=this.st_7
end on

on w_info_evolucioncondicion.destroy
call super::destroy
destroy(this.em_fechadesde)
destroy(this.st_12)
destroy(this.st_13)
destroy(this.em_fechahasta)
destroy(this.st_zona)
destroy(this.st_33)
destroy(this.cbx_todosfecha)
destroy(this.st_9)
destroy(this.st_11)
destroy(this.st_14)
destroy(this.st_1)
destroy(this.st_3)
destroy(this.ddlb_mercado)
destroy(this.cbx_mercado)
destroy(this.st_44)
destroy(this.st_6)
destroy(this.cbx_consmercado)
destroy(this.cbx_consfecha)
destroy(this.uo_muestrarecibidor)
destroy(this.uo_muestraespecies)
destroy(this.uo_muestravariedad)
destroy(this.uo_muestrazona)
destroy(this.uo_muestraproductor)
destroy(this.uo_selembalaje)
destroy(this.st_2)
destroy(this.st_4)
destroy(this.st_5)
destroy(this.st_7)
end on

event open;
x = 0
y = 0

This.Icon	=	Gstr_apl.Icono


//istr_mant.argumento[1] = codigo de mercado
Boolean	lb_Cerrar


IF IsNull(uo_muestrarecibidor.Codigo) THEN lb_Cerrar	=	True

IF IsNull(uo_muestraespecies.Codigo) THEN lb_Cerrar	=	True

IF IsNull(uo_muestravariedad.Codigo) THEN lb_Cerrar	=	True

IF IsNull(uo_muestrazona.Codigo) THEN lb_Cerrar	=	True

IF IsNull(uo_muestraproductor.Codigo) THEN lb_Cerrar	=	True

IF IsNull(uo_selembalaje.Codigo) THEN lb_Cerrar	=	True

IF lb_Cerrar THEN
	Close(This)
ELSE
	uo_muestrarecibidor.Seleccion(True, True)
	uo_muestraespecies.Seleccion(True, True)
	uo_muestravariedad.Seleccion(True, True)
	uo_muestrazona.Seleccion(True, True)
	uo_muestraproductor.Seleccion(True, True)
	uo_selembalaje.Seleccion(True, True)
	
	//ddlb_mercado.enabled    = FALSE
   cbx_consmercado.checked = FALSE


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
  uo_muestrazona.cbx_consolida.checked = FALSE

  //Productor
  uo_muestraproductor.cbx_consolida.checked = FALSE
  uo_muestraproductor.filtra(0)

  //Embalaje
  uo_selembalaje.cbx_consolida.checked = FALSE
  uo_selembalaje.filtra('U')

  uo_muestrarecibidor.dw_Seleccion.Object.codigo.BackGround.Color	=	RGB(255, 255, 255)
  uo_muestraespecies.dw_Seleccion.Object.codigo.BackGround.Color	=	RGB(255, 255, 255)	
  uo_muestravariedad.dw_Seleccion.Object.codigo.BackGround.Color	=	RGB(255, 255, 255)
  uo_muestrazona.dw_Seleccion.Object.codigo.BackGround.Color	=	RGB(255, 255, 255)	
  uo_muestraproductor.dw_Seleccion.Object.codigo.BackGround.Color	=	RGB(255, 255, 255)	
  uo_selembalaje.dw_Seleccion.Object.codigo.BackGround.Color	=	RGB(255, 255, 255)
  ddlb_mercado.backcolor = RGB(255, 255, 255)

  istr_mant.argumento[1] = ''
  em_fechadesde.text	  = String(Today())
  em_fechahasta.text	  = String(Today())
  cbx_consfecha.checked  = FALSE

END IF


end event

type st_computador from w_para_informes`st_computador within w_info_evolucioncondicion
end type

type st_usuario from w_para_informes`st_usuario within w_info_evolucioncondicion
end type

type st_temporada from w_para_informes`st_temporada within w_info_evolucioncondicion
end type

type p_logo from w_para_informes`p_logo within w_info_evolucioncondicion
end type

type st_titulo from w_para_informes`st_titulo within w_info_evolucioncondicion
integer x = 59
integer y = 64
integer width = 3141
string text = "Informe de Evolución de la Condición ( Origen - Destino )"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_evolucioncondicion
event mievento pbm_dwngraphcreate
string tag = "Imprimir Reporte"
integer x = 3319
integer y = 260
integer taborder = 140
string powertiptext = "Imprimir Reporte"
end type

event pb_acepta::mievento;//vinf.dw_1.Object.dw_2.SetSeriesStyle("gr_1", "tipo",LineColor! , RGB(192,192,192))
Return 1




end event

event pb_acepta::clicked;SetPointer(Arrow!)
Integer	li_fila, li_mercado, li_nave, li_recibidor, li_zona, li_especie, li_variedad, li_consfec 
Date		ld_FechaEmbaini, ld_FechaEmbafin
String   ls_embalaje, ls_tipotrans, ls_codreci, ls_nomreci
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



//Recibidor
IF uo_muestrarecibidor.cbx_consolida.checked THEN
	li_recibidor   = -9
	ls_codreci     = '99999'
	ls_nomreci     = 'Consolidado'
ELSE
   IF uo_muestrarecibidor.cbx_todos.checked THEN
      li_recibidor	= -1
		ls_codreci     = '99999'
	   ls_nomreci     = 'Todos'
		
   ELSE
      li_recibidor	= uo_muestrarecibidor.dw_Seleccion.Object.codigo[1]
	   IF IsNull(li_recibidor)THEN
	      MessageBox("Atención","Debe Seleccionar un Recibidor Previamente",Exclamation!)
	      RETURN
		ELSE
			ls_nomreci = captura_recibidor(li_recibidor)
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


//Fecha Embalaje
IF cbx_consfecha.Checked THEN
	li_consfec  = -9
	ld_FechaEmbaini =	Date(01/01/2000)
	ld_FechaEmbafin =	Today()
ELSE
  IF cbx_todosfecha.Checked THEN
	  ld_FechaEmbaini =	Date(01/01/2000)
	  ld_FechaEmbafin =	Today()
	  em_fechadesde.text = String(ld_FechaEmbaini,"dd/mm/yyyy")
	  em_fechahasta.Text = String(ld_FechaEmbafin,"dd/mm/yyyy")
	  li_consfec  = -1
  ELSE
    ld_FechaEmbaini = Date(em_fechadesde.Text)
	 ld_FechaEmbafin = Date(em_fechahasta.Text)
  END IF
END IF


istr_info.titulo	= 'INFORME DE EVOLUCIÓN DE CONDICIÓN (ORIGEN - DESTINO)'

OpenWithParm(vinf,istr_info)

vinf.dw_1.DataObject = "dw_info_evolucionuva"


vinf.dw_1.SetTransObject(sqlca)


li_fila = vinf.dw_1.Retrieve(gi_codexport,li_recibidor,li_especie,li_variedad,&
li_zona,ll_productor,ls_embalaje,ld_FechaEmbaini,ld_FechaEmbafin,li_consfec)

//vinf.dw_1.Object.dw_2.Dataobject.dw_info_evolucionuvadesgrane.Object("gr_1" , "tipo",Foreground! , RGB(196,196,196))
//Triggerevent("mievento")

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
	vinf.dw_1.Modify("cod_reci.text = '" + ls_codreci + "'")
	vinf.dw_1.Modify("nom_reci.text = '" + ls_nomreci + "'")
	IF gs_Ambiente <> 'Windows' THEN
		F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)

	END IF
END IF



SetPointer(Arrow!)

end event

type pb_salir from w_para_informes`pb_salir within w_info_evolucioncondicion
string tag = "Salir [Cerrar Ventana Activa]"
integer x = 3319
integer y = 540
integer taborder = 150
string powertiptext = "Salir [Cerrar Ventana Activa]"
end type

type em_fechadesde from editmask within w_info_evolucioncondicion
integer x = 1947
integer y = 764
integer width = 338
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

type st_12 from statictext within w_info_evolucioncondicion
integer x = 1957
integer y = 704
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
long backcolor = 12632256
boolean enabled = false
string text = "Desde"
boolean focusrectangle = false
end type

type st_13 from statictext within w_info_evolucioncondicion
integer x = 2514
integer y = 704
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
long backcolor = 12632256
boolean enabled = false
string text = "Hasta"
boolean focusrectangle = false
end type

type em_fechahasta from editmask within w_info_evolucioncondicion
integer x = 2510
integer y = 764
integer width = 338
integer height = 92
integer taborder = 130
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

type st_zona from statictext within w_info_evolucioncondicion
integer x = 1659
integer y = 264
integer width = 256
integer height = 140
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
boolean enabled = false
string text = "Zona Origen"
boolean focusrectangle = false
end type

type st_33 from statictext within w_info_evolucioncondicion
integer x = 1659
integer y = 460
integer width = 325
integer height = 84
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
boolean enabled = false
string text = "Productor"
boolean focusrectangle = false
end type

type cbx_todosfecha from checkbox within w_info_evolucioncondicion
integer x = 2871
integer y = 772
integer width = 137
integer height = 72
integer taborder = 100
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
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

type st_9 from statictext within w_info_evolucioncondicion
integer x = 91
integer y = 304
integer width = 288
integer height = 84
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
boolean enabled = false
string text = "Mercado"
boolean focusrectangle = false
end type

type st_11 from statictext within w_info_evolucioncondicion
integer x = 91
integer y = 480
integer width = 347
integer height = 84
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
boolean enabled = false
string text = "Recibidor"
boolean focusrectangle = false
end type

type st_14 from statictext within w_info_evolucioncondicion
integer x = 91
integer y = 628
integer width = 347
integer height = 84
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
boolean enabled = false
string text = "Especie"
boolean focusrectangle = false
end type

type st_1 from statictext within w_info_evolucioncondicion
integer x = 1659
integer y = 740
integer width = 274
integer height = 148
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
string text = "Fecha Embalaje"
boolean focusrectangle = false
end type

type st_3 from statictext within w_info_evolucioncondicion
integer x = 91
integer y = 776
integer width = 347
integer height = 84
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
string text = "Variedad"
boolean focusrectangle = false
end type

type ddlb_mercado from dropdownlistbox within w_info_evolucioncondicion
integer x = 416
integer y = 296
integer width = 878
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

type cbx_mercado from checkbox within w_info_evolucioncondicion
integer x = 1339
integer y = 312
integer width = 119
integer height = 64
integer taborder = 10
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
string text = " "
boolean checked = true
end type

event clicked;Integer	li_null

SetNull(li_null)
IF This.Checked THEN
	/*Nave*/
	//idwc_nave.Retrieve(0,'*')
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

type st_44 from statictext within w_info_evolucioncondicion
integer x = 59
integer y = 188
integer width = 3141
integer height = 732
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_6 from statictext within w_info_evolucioncondicion
integer x = 1659
integer y = 628
integer width = 311
integer height = 84
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
boolean enabled = false
string text = "Embalaje"
boolean focusrectangle = false
end type

type cbx_consmercado from checkbox within w_info_evolucioncondicion
integer x = 1536
integer y = 312
integer width = 110
integer height = 64
integer taborder = 20
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
string text = " "
end type

event clicked;Integer	li_null

SetNull(li_null)
IF This.Checked THEN
	/*Nave*/
	//idwc_nave.Retrieve(0,'*')
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

type cbx_consfecha from checkbox within w_info_evolucioncondicion
boolean visible = false
integer x = 3003
integer y = 1544
integer width = 101
integer height = 52
integer taborder = 110
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
boolean enabled = false
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

type uo_muestrarecibidor from uo_seleccion_recibidor_planilla_mod within w_info_evolucioncondicion
event destroy ( )
integer x = 416
integer y = 432
integer width = 1211
integer height = 132
integer taborder = 40
boolean bringtotop = true
end type

on uo_muestrarecibidor.destroy
call uo_seleccion_recibidor_planilla_mod::destroy
end on

type uo_muestraespecies from uo_seleccion_especie_mod within w_info_evolucioncondicion
event destroy ( )
integer x = 411
integer y = 588
integer width = 1211
integer taborder = 50
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

type uo_muestravariedad from uo_seleccion_variedad_mod within w_info_evolucioncondicion
integer x = 416
integer y = 744
integer width = 1211
integer height = 132
integer taborder = 60
boolean bringtotop = true
end type

on uo_muestravariedad.destroy
call uo_seleccion_variedad_mod::destroy
end on

type uo_muestrazona from uo_seleccion_zonas_mod within w_info_evolucioncondicion
integer x = 1947
integer y = 268
integer width = 1211
integer height = 132
integer taborder = 70
boolean bringtotop = true
end type

event ue_cambio;call super::ue_cambio;IF IsNull(This.Codigo) THEN RETURN

CHOOSE CASE This.Codigo
	CASE -1, -9
		uo_muestraproductor.Todos(True)
		
		//uo_muestraproductor.cbx_Todos.Enabled	=	False
		
	CASE ELSE
		uo_muestraproductor.Filtra(This.Codigo)
		
		uo_muestraproductor.cbx_Todos.Enabled	=	True
END CHOOSE
end event

on uo_muestrazona.destroy
call uo_seleccion_zonas_mod::destroy
end on

type uo_muestraproductor from uo_seleccion_productor_zonas_mod within w_info_evolucioncondicion
event destroy ( )
integer x = 1947
integer y = 424
integer width = 1211
integer height = 132
integer taborder = 80
boolean bringtotop = true
end type

on uo_muestraproductor.destroy
call uo_seleccion_productor_zonas_mod::destroy
end on

type uo_selembalaje from uo_seleccion_embalaje_mod within w_info_evolucioncondicion
event destroy ( )
integer x = 1947
integer y = 596
integer width = 1211
integer height = 116
integer taborder = 90
boolean bringtotop = true
end type

on uo_selembalaje.destroy
call uo_seleccion_embalaje_mod::destroy
end on

type st_2 from statictext within w_info_evolucioncondicion
integer x = 1262
integer y = 216
integer width = 224
integer height = 84
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
boolean enabled = false
string text = "Todos"
boolean focusrectangle = false
end type

type st_4 from statictext within w_info_evolucioncondicion
integer x = 1486
integer y = 216
integer width = 219
integer height = 56
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
boolean enabled = false
string text = "Cons."
boolean focusrectangle = false
end type

type st_5 from statictext within w_info_evolucioncondicion
integer x = 2793
integer y = 212
integer width = 206
integer height = 84
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
boolean enabled = false
string text = "Todos"
boolean focusrectangle = false
end type

type st_7 from statictext within w_info_evolucioncondicion
integer x = 3003
integer y = 212
integer width = 192
integer height = 56
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
boolean enabled = false
string text = "Cons."
boolean focusrectangle = false
end type

