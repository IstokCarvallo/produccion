$PBExportHeader$w_info_comparativo_origendestino.srw
$PBExportComments$Ventana de Informe de Resumen Objeciones
forward
global type w_info_comparativo_origendestino from w_para_informes
end type
type em_fechadesde from editmask within w_info_comparativo_origendestino
end type
type st_12 from statictext within w_info_comparativo_origendestino
end type
type st_13 from statictext within w_info_comparativo_origendestino
end type
type em_fechahasta from editmask within w_info_comparativo_origendestino
end type
type st_zona from statictext within w_info_comparativo_origendestino
end type
type st_33 from statictext within w_info_comparativo_origendestino
end type
type cbx_todosfecha from checkbox within w_info_comparativo_origendestino
end type
type st_11 from statictext within w_info_comparativo_origendestino
end type
type st_14 from statictext within w_info_comparativo_origendestino
end type
type st_3 from statictext within w_info_comparativo_origendestino
end type
type st_44 from statictext within w_info_comparativo_origendestino
end type
type st_5 from statictext within w_info_comparativo_origendestino
end type
type st_8 from statictext within w_info_comparativo_origendestino
end type
type cbx_consfecha from checkbox within w_info_comparativo_origendestino
end type
type uo_muestraespecies from uo_seleccion_especie_mod within w_info_comparativo_origendestino
end type
type uo_muestravariedad from uo_seleccion_variedad_mod within w_info_comparativo_origendestino
end type
type uo_muestrarecibidor from uo_seleccion_recibidor_planilla_mod within w_info_comparativo_origendestino
end type
type uo_muestrazona from uo_seleccion_zonas_mod within w_info_comparativo_origendestino
end type
type uo_muestraproductor from uo_seleccion_productor_zonas_mod within w_info_comparativo_origendestino
end type
type uo_selembalaje from uo_seleccion_embalaje_mod within w_info_comparativo_origendestino
end type
type st_2 from statictext within w_info_comparativo_origendestino
end type
type st_4 from statictext within w_info_comparativo_origendestino
end type
type st_9 from statictext within w_info_comparativo_origendestino
end type
type st_10 from statictext within w_info_comparativo_origendestino
end type
type st_1 from statictext within w_info_comparativo_origendestino
end type
type st_15 from statictext within w_info_comparativo_origendestino
end type
type st_6 from statictext within w_info_comparativo_origendestino
end type
type uo_muestranave from uo_seleccion_naves_mod within w_info_comparativo_origendestino
end type
type uo_muestradestino from uo_seleccion_destinos_mod within w_info_comparativo_origendestino
end type
type uo_muestracalibre from uo_seleccion_calibre_mod within w_info_comparativo_origendestino
end type
end forward

global type w_info_comparativo_origendestino from w_para_informes
integer x = 14
integer y = 32
integer width = 2217
integer height = 1808
string title = "Informe Comparativo Origen-Destino"
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
st_11 st_11
st_14 st_14
st_3 st_3
st_44 st_44
st_5 st_5
st_8 st_8
cbx_consfecha cbx_consfecha
uo_muestraespecies uo_muestraespecies
uo_muestravariedad uo_muestravariedad
uo_muestrarecibidor uo_muestrarecibidor
uo_muestrazona uo_muestrazona
uo_muestraproductor uo_muestraproductor
uo_selembalaje uo_selembalaje
st_2 st_2
st_4 st_4
st_9 st_9
st_10 st_10
st_1 st_1
st_15 st_15
st_6 st_6
uo_muestranave uo_muestranave
uo_muestradestino uo_muestradestino
uo_muestracalibre uo_muestracalibre
end type
global w_info_comparativo_origendestino w_info_comparativo_origendestino

type variables
str_busqueda istr_busq
str_mant istr_mant
Integer	ii_tipo
String	is_report, is_nula

DataWindowChild		idwc_nave, idwc_planilla





end variables

forward prototypes
public function boolean noexistevariecalibre (integer variedad, string calibre)
public function integer filtra_recibidor (integer ai_destino)
public function integer nave ()
end prototypes

public function boolean noexistevariecalibre (integer variedad, string calibre);String ls_Codigo

Calibre	= Calibre + Fill(" ",3 - Len(Calibre))
ls_codigo	=	''



	SELECT Max(vaca_calibr)
		INTO	:ls_Codigo
		FROM	dba.variecalibre
		WHERE espe_codigo =  :gi_CodEspecie
		AND   :variedad in (0,vari_codigo)
		AND   vaca_calibr	=	:Calibre;
	
	IF sqlca.sqlcode = -1 THEN
		F_ErrorBaseDatos(sqlca,"No se pudo leer la tabla VarieCalibre")
		RETURN TRUE
	ELSEIF ls_Codigo = '' OR IsNull(ls_codigo) THEN
		MessageBox("Atencion","Código de Calibre no Existe, Ingrese Otro Código",Exclamation!)
		RETURN TRUE
	ELSE	
		RETURN FALSE	
	END IF

		
end function

public function integer filtra_recibidor (integer ai_destino);Integer li_mercado

IF IsNull(li_mercado) THEN li_mercado = 0

SELECT merc_codigo 
INTO   :li_mercado
FROM   dba.destinos
WHERE  dest_codigo = :ai_destino;

RETURN li_mercado
end function

public function integer nave ();Date    ld_fecha
Integer li_nave

SELECT max(cpde_fecarr) 
INTO :ld_fecha
FROM dba.ctlcalplandestinosenc;

SELECT DISTINCT nave_codigo
INTO : li_nave
FROM dba.ctlcalplandestinosenc
WHERE cpde_fecarr = :ld_fecha;

IF IsNull(li_nave) THEN li_nave = 0


RETURN li_nave
end function

on w_info_comparativo_origendestino.create
int iCurrent
call super::create
this.em_fechadesde=create em_fechadesde
this.st_12=create st_12
this.st_13=create st_13
this.em_fechahasta=create em_fechahasta
this.st_zona=create st_zona
this.st_33=create st_33
this.cbx_todosfecha=create cbx_todosfecha
this.st_11=create st_11
this.st_14=create st_14
this.st_3=create st_3
this.st_44=create st_44
this.st_5=create st_5
this.st_8=create st_8
this.cbx_consfecha=create cbx_consfecha
this.uo_muestraespecies=create uo_muestraespecies
this.uo_muestravariedad=create uo_muestravariedad
this.uo_muestrarecibidor=create uo_muestrarecibidor
this.uo_muestrazona=create uo_muestrazona
this.uo_muestraproductor=create uo_muestraproductor
this.uo_selembalaje=create uo_selembalaje
this.st_2=create st_2
this.st_4=create st_4
this.st_9=create st_9
this.st_10=create st_10
this.st_1=create st_1
this.st_15=create st_15
this.st_6=create st_6
this.uo_muestranave=create uo_muestranave
this.uo_muestradestino=create uo_muestradestino
this.uo_muestracalibre=create uo_muestracalibre
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.em_fechadesde
this.Control[iCurrent+2]=this.st_12
this.Control[iCurrent+3]=this.st_13
this.Control[iCurrent+4]=this.em_fechahasta
this.Control[iCurrent+5]=this.st_zona
this.Control[iCurrent+6]=this.st_33
this.Control[iCurrent+7]=this.cbx_todosfecha
this.Control[iCurrent+8]=this.st_11
this.Control[iCurrent+9]=this.st_14
this.Control[iCurrent+10]=this.st_3
this.Control[iCurrent+11]=this.st_44
this.Control[iCurrent+12]=this.st_5
this.Control[iCurrent+13]=this.st_8
this.Control[iCurrent+14]=this.cbx_consfecha
this.Control[iCurrent+15]=this.uo_muestraespecies
this.Control[iCurrent+16]=this.uo_muestravariedad
this.Control[iCurrent+17]=this.uo_muestrarecibidor
this.Control[iCurrent+18]=this.uo_muestrazona
this.Control[iCurrent+19]=this.uo_muestraproductor
this.Control[iCurrent+20]=this.uo_selembalaje
this.Control[iCurrent+21]=this.st_2
this.Control[iCurrent+22]=this.st_4
this.Control[iCurrent+23]=this.st_9
this.Control[iCurrent+24]=this.st_10
this.Control[iCurrent+25]=this.st_1
this.Control[iCurrent+26]=this.st_15
this.Control[iCurrent+27]=this.st_6
this.Control[iCurrent+28]=this.uo_muestranave
this.Control[iCurrent+29]=this.uo_muestradestino
this.Control[iCurrent+30]=this.uo_muestracalibre
end on

on w_info_comparativo_origendestino.destroy
call super::destroy
destroy(this.em_fechadesde)
destroy(this.st_12)
destroy(this.st_13)
destroy(this.em_fechahasta)
destroy(this.st_zona)
destroy(this.st_33)
destroy(this.cbx_todosfecha)
destroy(this.st_11)
destroy(this.st_14)
destroy(this.st_3)
destroy(this.st_44)
destroy(this.st_5)
destroy(this.st_8)
destroy(this.cbx_consfecha)
destroy(this.uo_muestraespecies)
destroy(this.uo_muestravariedad)
destroy(this.uo_muestrarecibidor)
destroy(this.uo_muestrazona)
destroy(this.uo_muestraproductor)
destroy(this.uo_selembalaje)
destroy(this.st_2)
destroy(this.st_4)
destroy(this.st_9)
destroy(this.st_10)
destroy(this.st_1)
destroy(this.st_15)
destroy(this.st_6)
destroy(this.uo_muestranave)
destroy(this.uo_muestradestino)
destroy(this.uo_muestracalibre)
end on

event open;
x = 0
y = 0

This.Icon	=	Gstr_apl.Icono

Boolean	lb_Cerrar

IF IsNull(uo_muestraespecies.Codigo) THEN lb_Cerrar	=	True
IF IsNull(uo_muestravariedad.Codigo) THEN lb_Cerrar	=	True
IF IsNull(uo_muestrarecibidor.Codigo) THEN lb_Cerrar	=	True
IF IsNull(uo_muestrazona.Codigo) THEN lb_Cerrar	=	True
IF IsNull(uo_muestraproductor.Codigo) THEN lb_Cerrar	=	True
IF IsNull(uo_selembalaje.Codigo) THEN lb_Cerrar	=	True
IF IsNull(uo_muestranave.Codigo) THEN lb_Cerrar	=	True
IF IsNull(uo_muestradestino.Codigo) THEN lb_Cerrar	=	True
IF IsNull(uo_muestracalibre.Codigo) THEN lb_Cerrar	=	True


IF lb_Cerrar THEN
	Close(This)
ELSE
	uo_muestraespecies.Seleccion(True, True)
	uo_muestravariedad.Seleccion(True, True)
	uo_muestrarecibidor.Seleccion(True, True)
	uo_muestrazona.Seleccion(True, True)
	uo_muestraproductor.Seleccion(True, True)
	uo_selembalaje.Seleccion(True, True)
	uo_muestranave.Seleccion(True, True)
	uo_muestradestino.Seleccion(True, True)
	uo_muestracalibre.Seleccion(True, True)


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


//Recibidor
uo_muestrarecibidor.cbx_consolida.checked = FALSE
uo_muestrarecibidor.cbx_todos.checked = TRUE

//Zona
uo_muestrazona.cbx_consolida.checked = FALSE

//Productor
uo_muestraproductor.cbx_consolida.checked = FALSE
uo_muestraproductor.filtra(0)

//Embalaje
uo_selembalaje.cbx_consolida.checked = FALSE
uo_selembalaje.filtra('U')

//Nave
uo_muestranave.filtra('M')
IF nave() = 0 THEN
	uo_muestranave.cbx_todos.checked = TRUE
ELSE
 uo_muestranave.dw_seleccion.object.codigo[1] = nave()
 uo_muestranave.cbx_todos.checked = FALSE
END IF

uo_muestraespecies.dw_Seleccion.Object.codigo.BackGround.Color	=	RGB(255, 255, 255)	
uo_muestravariedad.dw_Seleccion.Object.codigo.BackGround.Color	=	RGB(255, 255, 255)
uo_muestrarecibidor.dw_Seleccion.Object.codigo.BackGround.Color	=	RGB(255, 255, 255)
uo_muestrazona.dw_Seleccion.Object.codigo.BackGround.Color	=	RGB(255, 255, 255)	
uo_muestraproductor.dw_Seleccion.Object.codigo.BackGround.Color	=	RGB(255, 255, 255)	
uo_selembalaje.dw_Seleccion.Object.codigo.BackGround.Color	=	RGB(255, 255, 255)

em_fechadesde.text	  = String(Today())
em_fechahasta.text	  = String(Today())

END IF
end event

type st_computador from w_para_informes`st_computador within w_info_comparativo_origendestino
end type

type st_usuario from w_para_informes`st_usuario within w_info_comparativo_origendestino
end type

type st_temporada from w_para_informes`st_temporada within w_info_comparativo_origendestino
end type

type p_logo from w_para_informes`p_logo within w_info_comparativo_origendestino
end type

type st_titulo from w_para_informes`st_titulo within w_info_comparativo_origendestino
integer x = 32
integer y = 64
integer width = 1783
string text = "Informe Comparativo Origen-Destino"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_comparativo_origendestino
string tag = "Imprimir Reporte"
integer x = 1929
integer y = 248
integer taborder = 140
string powertiptext = "Imprimir Reporte"
end type

event pb_acepta::clicked;SetPointer(Arrow!)
Integer	li_fila, li_zona, li_especie, li_variedad, li_nave, li_destino, li_consfecha 
Date		ld_FechaEmbaini, ld_FechaEmbafin
String   ls_embalaje, ls_calibre
Long     ll_Productor, ll_recibidor 

SetPointer(HourGlass!)

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

//Recibidor

IF uo_muestrarecibidor.cbx_consolida.checked THEN
	ll_recibidor   = -9
ELSE
   IF uo_muestrarecibidor.cbx_todos.checked THEN
      ll_recibidor	= -1
   ELSE
      ll_recibidor	= uo_muestrarecibidor.dw_Seleccion.Object.codigo[1]
	   IF IsNull(ll_recibidor)THEN
	      MessageBox("Atención","Debe Seleccionar un Recibidor Previamente",Exclamation!)
	      RETURN
	   END IF
   END IF
END IF


//zona
IF uo_muestrazona.cbx_consolida.checked THEN
	li_zona   = -9
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

//Embalaje
   IF uo_selembalaje.cbx_todos.checked THEN
	   ls_embalaje = '*'
   ELSE
      ls_embalaje = uo_selembalaje.dw_Seleccion.Object.codigo[1]
	   IF IsNull(ls_embalaje)THEN
	      MessageBox("Atención","Debe Seleccionar un Embalaje Previamente",Exclamation!)
	      RETURN
	   END IF
   END IF

//Calibre
  IF uo_muestracalibre.cbx_todos.checked THEN
	   ls_calibre = '*'
   ELSE
      ls_calibre = uo_muestracalibre.dw_Seleccion.Object.codigo[1]
	   IF IsNull(ls_calibre)THEN
	      MessageBox("Atención","Debe Seleccionar un Calibre Previamente",Exclamation!)
	      RETURN
	   END IF
   END IF

//Nave
IF uo_muestranave.cbx_consolida.checked THEN
	li_nave = -9
ELSE
   IF uo_muestranave.cbx_todos.checked THEN
	   li_nave = -1
   ELSE
      li_nave = uo_muestranave.dw_Seleccion.Object.codigo[1]
	   IF IsNull(li_nave)THEN
	      MessageBox("Atención","Debe Seleccionar una Nave Previamente",Exclamation!)
	      RETURN
	   END IF
   END IF
END IF

//Destino
IF uo_muestradestino.cbx_consolida.checked THEN
	li_destino = -9
ELSE
   IF uo_muestradestino.cbx_todos.checked THEN
	   li_destino = -1
   ELSE
      li_destino = uo_muestradestino.dw_Seleccion.Object.codigo[1]
	   IF IsNull(li_destino)THEN
	      MessageBox("Atención","Debe Seleccionar un Destino Previamente",Exclamation!)
	      RETURN
	   END IF
   END IF
END IF
	
//Fecha Embalaje
IF cbx_consfecha.Checked THEN
	ld_FechaEmbaini =	Date(01/01/2000)
	ld_FechaEmbafin =	Today()
	li_consfecha = -9
ELSE
  IF cbx_todosfecha.Checked THEN
	  ld_FechaEmbaini =	Date(01/01/2000)
	  ld_FechaEmbafin =	Today()
	  em_fechadesde.text = String(ld_FechaEmbaini,"dd/mm/yyyy")
	  em_fechahasta.Text = String(ld_FechaEmbafin,"dd/mm/yyyy")
  ELSE
    ld_FechaEmbaini = Date(em_fechadesde.Text)
	 ld_FechaEmbafin = Date(em_fechahasta.Text)
  END IF
END IF


istr_info.titulo	= 'COMPARATIVO ORIGEN DESTINO'

OpenWithParm(vinf,istr_info)

vinf.dw_1.DataObject = "dw_info_comparativo_oridest"


vinf.dw_1.SetTransObject(sqlca)

li_fila = vinf.dw_1.Retrieve(li_especie,li_variedad,li_zona,ll_productor,&
li_destino,ll_recibidor,li_nave,ld_FechaEmbaini,ld_FechaEmbafin,ls_embalaje,&
ls_calibre,li_consfecha)


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
	vinf.dw_1.Object.DataWindow.Zoom = 65
	
	IF gs_Ambiente <> 'Windows' THEN
		F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)

	END IF
END IF

SetPointer(Arrow!)

end event

type pb_salir from w_para_informes`pb_salir within w_info_comparativo_origendestino
string tag = "Salir [Cerrar Ventana Activa]"
integer x = 1929
integer y = 528
integer taborder = 150
string powertiptext = "Salir [Cerrar Ventana Activa]"
end type

type em_fechadesde from editmask within w_info_comparativo_origendestino
integer x = 407
integer y = 1236
integer width = 347
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

type st_12 from statictext within w_info_comparativo_origendestino
integer x = 425
integer y = 1172
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
long backcolor = 12632256
boolean enabled = false
string text = "Desde"
boolean focusrectangle = false
end type

type st_13 from statictext within w_info_comparativo_origendestino
integer x = 969
integer y = 1172
integer width = 178
integer height = 60
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

type em_fechahasta from editmask within w_info_comparativo_origendestino
integer x = 960
integer y = 1236
integer width = 347
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

type st_zona from statictext within w_info_comparativo_origendestino
integer x = 82
integer y = 488
integer width = 261
integer height = 76
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
boolean enabled = false
string text = "Zona"
boolean focusrectangle = false
end type

type st_33 from statictext within w_info_comparativo_origendestino
integer x = 82
integer y = 600
integer width = 315
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

type cbx_todosfecha from checkbox within w_info_comparativo_origendestino
integer x = 1335
integer y = 1244
integer width = 114
integer height = 72
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
string text = " "
boolean checked = true
end type

event clicked;IF This.Checked THEN
	em_fechadesde.Enabled	=	FALSE
	em_fechahasta.Enabled	=	FALSE
   istr_Mant.Argumento[12]	=	'01/01/1900'
	istr_Mant.Argumento[13]	=	String(Today())
	//cbx_consfecha.Enabled	=	TRUE
	cbx_consfecha.Checked = False
ELSE
	em_fechadesde.Enabled	=	TRUE
	em_fechahasta.Enabled	=	TRUE
	//cbx_consfecha.Enabled	=	FALSE
	//cbx_consfecha.Checked	=	FALSE
	istr_Mant.Argumento[12] =	em_fechadesde.Text
	istr_Mant.Argumento[13]	=	em_fechahasta.Text
	em_fechadesde.Setfocus()
END IF
RETURN 0





end event

type st_11 from statictext within w_info_comparativo_origendestino
integer x = 82
integer y = 828
integer width = 315
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

type st_14 from statictext within w_info_comparativo_origendestino
integer x = 82
integer y = 276
integer width = 233
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

type st_3 from statictext within w_info_comparativo_origendestino
integer x = 78
integer y = 384
integer width = 297
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

type st_44 from statictext within w_info_comparativo_origendestino
integer x = 32
integer y = 172
integer width = 1783
integer height = 904
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

type st_5 from statictext within w_info_comparativo_origendestino
integer x = 91
integer y = 1416
integer width = 389
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

type st_8 from statictext within w_info_comparativo_origendestino
integer x = 91
integer y = 1540
integer width = 270
integer height = 84
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
boolean enabled = false
string text = "Calibre"
boolean focusrectangle = false
end type

type cbx_consfecha from checkbox within w_info_comparativo_origendestino
integer x = 1545
integer y = 1244
integer width = 142
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
end type

event clicked;IF This.Checked THEN
	em_fechadesde.Enabled	=	FALSE
	em_fechahasta.Enabled	=	FALSE
   istr_Mant.Argumento[12]	=	'01/01/1900'
	istr_Mant.Argumento[13]	=	String(Today())
	//cbx_consfecha.Enabled	=	TRUE
	cbx_todosfecha.Checked = False
ELSE
	em_fechadesde.Enabled	=	TRUE
	em_fechahasta.Enabled	=	TRUE
	//cbx_consfecha.Enabled	=	FALSE
	//cbx_consfecha.Checked	=	FALSE
	istr_Mant.Argumento[12] =	em_fechadesde.Text
	istr_Mant.Argumento[13]	=	em_fechahasta.Text
	em_fechadesde.Setfocus()
END IF
RETURN 0
end event

type uo_muestraespecies from uo_seleccion_especie_mod within w_info_comparativo_origendestino
integer x = 411
integer y = 244
integer width = 1216
integer taborder = 10
boolean bringtotop = true
end type

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

on uo_muestraespecies.destroy
call uo_seleccion_especie_mod::destroy
end on

type uo_muestravariedad from uo_seleccion_variedad_mod within w_info_comparativo_origendestino
integer x = 411
integer y = 348
integer width = 1234
integer taborder = 20
boolean bringtotop = true
end type

on uo_muestravariedad.destroy
call uo_seleccion_variedad_mod::destroy
end on

event ue_cambio;call super::ue_cambio;IF IsNull(This.Codigo) THEN RETURN

CHOOSE CASE This.Codigo
	CASE -1, -9
		uo_muestracalibre.Todos(True)
	CASE ELSE
		uo_muestracalibre.Filtra(Gi_CodExport,uo_muestraespecies.dw_Seleccion.Object.codigo[1],&
		                         This.Codigo)
		
END CHOOSE
end event

type uo_muestrarecibidor from uo_seleccion_recibidor_planilla_mod within w_info_comparativo_origendestino
integer x = 421
integer y = 792
integer width = 1225
integer taborder = 30
boolean bringtotop = true
end type

on uo_muestrarecibidor.destroy
call uo_seleccion_recibidor_planilla_mod::destroy
end on

type uo_muestrazona from uo_seleccion_zonas_mod within w_info_comparativo_origendestino
integer x = 411
integer y = 460
integer width = 1221
integer taborder = 40
boolean bringtotop = true
end type

event ue_cambio;call super::ue_cambio;IF IsNull(This.Codigo) THEN RETURN

CHOOSE CASE This.Codigo
	CASE -1, -9
		uo_muestraproductor.Todos(True)
	CASE ELSE
		uo_muestraproductor.Filtra(This.Codigo)
		
END CHOOSE
	
end event

on uo_muestrazona.destroy
call uo_seleccion_zonas_mod::destroy
end on

type uo_muestraproductor from uo_seleccion_productor_zonas_mod within w_info_comparativo_origendestino
integer x = 416
integer y = 568
integer width = 1243
integer taborder = 50
boolean bringtotop = true
end type

on uo_muestraproductor.destroy
call uo_seleccion_productor_zonas_mod::destroy
end on

type uo_selembalaje from uo_seleccion_embalaje_mod within w_info_comparativo_origendestino
integer x = 416
integer y = 1380
integer width = 1038
integer taborder = 60
boolean bringtotop = true
end type

on uo_selembalaje.destroy
call uo_seleccion_embalaje_mod::destroy
end on

type st_2 from statictext within w_info_comparativo_origendestino
integer x = 1248
integer y = 180
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

type st_4 from statictext within w_info_comparativo_origendestino
integer x = 1467
integer y = 180
integer width = 215
integer height = 84
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

type st_9 from statictext within w_info_comparativo_origendestino
integer x = 82
integer y = 716
integer width = 302
integer height = 68
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
boolean enabled = false
string text = "Destino"
boolean focusrectangle = false
end type

type st_10 from statictext within w_info_comparativo_origendestino
integer x = 82
integer y = 944
integer width = 192
integer height = 72
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
boolean enabled = false
string text = "Nave"
boolean focusrectangle = false
end type

type st_1 from statictext within w_info_comparativo_origendestino
integer x = 78
integer y = 1104
integer width = 402
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
string text = "Fecha Arribo"
boolean focusrectangle = false
end type

type st_15 from statictext within w_info_comparativo_origendestino
integer x = 32
integer y = 1076
integer width = 1783
integer height = 292
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_6 from statictext within w_info_comparativo_origendestino
integer x = 32
integer y = 1368
integer width = 1783
integer height = 296
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type uo_muestranave from uo_seleccion_naves_mod within w_info_comparativo_origendestino
integer x = 425
integer y = 912
integer taborder = 40
boolean bringtotop = true
end type

on uo_muestranave.destroy
call uo_seleccion_naves_mod::destroy
end on

type uo_muestradestino from uo_seleccion_destinos_mod within w_info_comparativo_origendestino
integer x = 421
integer y = 680
integer taborder = 60
boolean bringtotop = true
end type

on uo_muestradestino.destroy
call uo_seleccion_destinos_mod::destroy
end on

event ue_cambio;call super::ue_cambio;IF IsNull(This.Codigo) THEN RETURN

CHOOSE CASE This.Codigo
	CASE -1, -9
		uo_muestrarecibidor.Todos(True)
		
	CASE ELSE
		uo_muestrarecibidor.Filtra(filtra_recibidor(This.Codigo))
		
END CHOOSE
	
end event

type uo_muestracalibre from uo_seleccion_calibre_mod within w_info_comparativo_origendestino
integer x = 416
integer y = 1500
integer width = 1056
integer taborder = 70
boolean bringtotop = true
end type

on uo_muestracalibre.destroy
call uo_seleccion_calibre_mod::destroy
end on

