$PBExportHeader$w_inf_recepcion_tipo_embarque_ooo.srw
$PBExportComments$Ventana de Informe de Resumen de palnillas de evaluación de descarte de fruta de packing
forward
global type w_inf_recepcion_tipo_embarque_ooo from w_para_informes
end type
type em_fechadesde from editmask within w_inf_recepcion_tipo_embarque_ooo
end type
type st_12 from statictext within w_inf_recepcion_tipo_embarque_ooo
end type
type st_13 from statictext within w_inf_recepcion_tipo_embarque_ooo
end type
type em_fechahasta from editmask within w_inf_recepcion_tipo_embarque_ooo
end type
type st_zona from statictext within w_inf_recepcion_tipo_embarque_ooo
end type
type st_33 from statictext within w_inf_recepcion_tipo_embarque_ooo
end type
type cbx_todosfecha from checkbox within w_inf_recepcion_tipo_embarque_ooo
end type
type st_14 from statictext within w_inf_recepcion_tipo_embarque_ooo
end type
type cbx_consfecha from checkbox within w_inf_recepcion_tipo_embarque_ooo
end type
type st_2 from statictext within w_inf_recepcion_tipo_embarque_ooo
end type
type uo_muestrazona from uo_seleccion_zonas_mod within w_inf_recepcion_tipo_embarque_ooo
end type
type st_1 from statictext within w_inf_recepcion_tipo_embarque_ooo
end type
type st_4 from statictext within w_inf_recepcion_tipo_embarque_ooo
end type
type uo_muestravariedad from uo_seleccion_variedad_mod within w_inf_recepcion_tipo_embarque_ooo
end type
type st_3 from statictext within w_inf_recepcion_tipo_embarque_ooo
end type
type uo_muestraproductor from uo_seleccion_productor_mod within w_inf_recepcion_tipo_embarque_ooo
end type
type st_7 from statictext within w_inf_recepcion_tipo_embarque_ooo
end type
type st_9 from statictext within w_inf_recepcion_tipo_embarque_ooo
end type
type ddlb_exporta from dropdownlistbox within w_inf_recepcion_tipo_embarque_ooo
end type
type ddlb_embarque from dropdownlistbox within w_inf_recepcion_tipo_embarque_ooo
end type
type gb_3 from groupbox within w_inf_recepcion_tipo_embarque_ooo
end type
type st_10 from statictext within w_inf_recepcion_tipo_embarque_ooo
end type
type uo_muestraplanta from uo_seleccion_planta_mod within w_inf_recepcion_tipo_embarque_ooo
end type
type gb_4 from groupbox within w_inf_recepcion_tipo_embarque_ooo
end type
type uo_muestrapredio from uo_seleccion_predioproduc_mod within w_inf_recepcion_tipo_embarque_ooo
end type
type gb_5 from groupbox within w_inf_recepcion_tipo_embarque_ooo
end type
type rb_tbrix from radiobutton within w_inf_recepcion_tipo_embarque_ooo
end type
type rb_mybrix from radiobutton within w_inf_recepcion_tipo_embarque_ooo
end type
type rb_mebrix from radiobutton within w_inf_recepcion_tipo_embarque_ooo
end type
type gb_6 from groupbox within w_inf_recepcion_tipo_embarque_ooo
end type
type st_44 from statictext within w_inf_recepcion_tipo_embarque_ooo
end type
type rb_tfirmeza from radiobutton within w_inf_recepcion_tipo_embarque_ooo
end type
type rb_myfirmeza from radiobutton within w_inf_recepcion_tipo_embarque_ooo
end type
type rb_mefirmeza from radiobutton within w_inf_recepcion_tipo_embarque_ooo
end type
end forward

global type w_inf_recepcion_tipo_embarque_ooo from w_para_informes
string tag = "Consulta Planilla Recepción Tipo Embarque"
integer x = 14
integer y = 32
integer width = 3104
integer height = 1980
string title = "INFORME RESUMEN RECEPCION CEREZAS"
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
st_14 st_14
cbx_consfecha cbx_consfecha
st_2 st_2
uo_muestrazona uo_muestrazona
st_1 st_1
st_4 st_4
uo_muestravariedad uo_muestravariedad
st_3 st_3
uo_muestraproductor uo_muestraproductor
st_7 st_7
st_9 st_9
ddlb_exporta ddlb_exporta
ddlb_embarque ddlb_embarque
gb_3 gb_3
st_10 st_10
uo_muestraplanta uo_muestraplanta
gb_4 gb_4
uo_muestrapredio uo_muestrapredio
gb_5 gb_5
rb_tbrix rb_tbrix
rb_mybrix rb_mybrix
rb_mebrix rb_mebrix
gb_6 gb_6
st_44 st_44
rb_tfirmeza rb_tfirmeza
rb_myfirmeza rb_myfirmeza
rb_mefirmeza rb_mefirmeza
end type
global w_inf_recepcion_tipo_embarque_ooo w_inf_recepcion_tipo_embarque_ooo

type variables
str_busqueda      istr_busq
str_mant         istr_mant
     
Integer		ii_tipo, ii_especie
String		is_report, is_nula


end variables

forward prototypes
public subroutine ubicacion ()
end prototypes

public subroutine ubicacion ();gb_4.height		=	gb_4.height - 100
st_44.height  	=  st_44.height - 100
rb_tbrix.y	=	896
rb_mybrix.y	=	896
rb_mebrix.y	=	896
gb_5.y		=	800
gb_6.y		=	1040
rb_tfirmeza.y	=	1132
rb_myfirmeza.y	=	1132
rb_mefirmeza.y	=	1132
st_10.y			=	1300
gb_3.y			=	1312	
st_4.y			=	1432	
st_12.y			=	1364	
st_13.y			=	1364
em_fechadesde.y =  1428
em_fechahasta.y =  1428


end subroutine

on w_inf_recepcion_tipo_embarque_ooo.create
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
this.cbx_consfecha=create cbx_consfecha
this.st_2=create st_2
this.uo_muestrazona=create uo_muestrazona
this.st_1=create st_1
this.st_4=create st_4
this.uo_muestravariedad=create uo_muestravariedad
this.st_3=create st_3
this.uo_muestraproductor=create uo_muestraproductor
this.st_7=create st_7
this.st_9=create st_9
this.ddlb_exporta=create ddlb_exporta
this.ddlb_embarque=create ddlb_embarque
this.gb_3=create gb_3
this.st_10=create st_10
this.uo_muestraplanta=create uo_muestraplanta
this.gb_4=create gb_4
this.uo_muestrapredio=create uo_muestrapredio
this.gb_5=create gb_5
this.rb_tbrix=create rb_tbrix
this.rb_mybrix=create rb_mybrix
this.rb_mebrix=create rb_mebrix
this.gb_6=create gb_6
this.st_44=create st_44
this.rb_tfirmeza=create rb_tfirmeza
this.rb_myfirmeza=create rb_myfirmeza
this.rb_mefirmeza=create rb_mefirmeza
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.em_fechadesde
this.Control[iCurrent+2]=this.st_12
this.Control[iCurrent+3]=this.st_13
this.Control[iCurrent+4]=this.em_fechahasta
this.Control[iCurrent+5]=this.st_zona
this.Control[iCurrent+6]=this.st_33
this.Control[iCurrent+7]=this.cbx_todosfecha
this.Control[iCurrent+8]=this.st_14
this.Control[iCurrent+9]=this.cbx_consfecha
this.Control[iCurrent+10]=this.st_2
this.Control[iCurrent+11]=this.uo_muestrazona
this.Control[iCurrent+12]=this.st_1
this.Control[iCurrent+13]=this.st_4
this.Control[iCurrent+14]=this.uo_muestravariedad
this.Control[iCurrent+15]=this.st_3
this.Control[iCurrent+16]=this.uo_muestraproductor
this.Control[iCurrent+17]=this.st_7
this.Control[iCurrent+18]=this.st_9
this.Control[iCurrent+19]=this.ddlb_exporta
this.Control[iCurrent+20]=this.ddlb_embarque
this.Control[iCurrent+21]=this.gb_3
this.Control[iCurrent+22]=this.st_10
this.Control[iCurrent+23]=this.uo_muestraplanta
this.Control[iCurrent+24]=this.gb_4
this.Control[iCurrent+25]=this.uo_muestrapredio
this.Control[iCurrent+26]=this.gb_5
this.Control[iCurrent+27]=this.rb_tbrix
this.Control[iCurrent+28]=this.rb_mybrix
this.Control[iCurrent+29]=this.rb_mebrix
this.Control[iCurrent+30]=this.gb_6
this.Control[iCurrent+31]=this.st_44
this.Control[iCurrent+32]=this.rb_tfirmeza
this.Control[iCurrent+33]=this.rb_myfirmeza
this.Control[iCurrent+34]=this.rb_mefirmeza
end on

on w_inf_recepcion_tipo_embarque_ooo.destroy
call super::destroy
destroy(this.em_fechadesde)
destroy(this.st_12)
destroy(this.st_13)
destroy(this.em_fechahasta)
destroy(this.st_zona)
destroy(this.st_33)
destroy(this.cbx_todosfecha)
destroy(this.st_14)
destroy(this.cbx_consfecha)
destroy(this.st_2)
destroy(this.uo_muestrazona)
destroy(this.st_1)
destroy(this.st_4)
destroy(this.uo_muestravariedad)
destroy(this.st_3)
destroy(this.uo_muestraproductor)
destroy(this.st_7)
destroy(this.st_9)
destroy(this.ddlb_exporta)
destroy(this.ddlb_embarque)
destroy(this.gb_3)
destroy(this.st_10)
destroy(this.uo_muestraplanta)
destroy(this.gb_4)
destroy(this.uo_muestrapredio)
destroy(this.gb_5)
destroy(this.rb_tbrix)
destroy(this.rb_mybrix)
destroy(this.rb_mebrix)
destroy(this.gb_6)
destroy(this.st_44)
destroy(this.rb_tfirmeza)
destroy(this.rb_myfirmeza)
destroy(this.rb_mefirmeza)
end on

event open;x = 0
y = 0
This.Icon	=	Gstr_apl.Icono

String ls_especie
Boolean	lb_Cerrar

ls_especie	= Message.StringParm
ii_especie	= Integer(ls_especie)

CHOOSE CASE ls_especie
	CASE '21'
		This.Title	= "INFORME RESUMEN RECEPCION CEREZAS"
	CASE '41'
		This.Title  = "INFORME RESUMEN RECEPCION KIWIS"
		rb_mybrix.text = 'Mayor a 5,2º'
		rb_mebrix.text = 'Menor a  5,2º'
		rb_myfirmeza.text = 'Mayor a 6 Lb'
		rb_mefirmeza.text = 'Menor a  6 Lb'
		st_9.visible		=	FALSE
		ddlb_embarque.visible =	FALSE
		
END CHOOSE

IF IsNull(uo_muestravariedad.Codigo) THEN lb_Cerrar	=	True

IF IsNull(uo_muestrazona.Codigo) THEN lb_Cerrar	=	True

IF IsNull(uo_muestraplanta.Codigo) THEN lb_Cerrar	=	True

IF IsNull(uo_muestraproductor.Codigo) THEN lb_Cerrar	=	True

//IF IsNull(uo_muestrapredio.Codigo) THEN lb_Cerrar	=	False

IF lb_Cerrar THEN
	Close(This)
ELSE
	uo_muestravariedad.Seleccion(True, True)
	uo_muestrazona.Seleccion(True, True)
	uo_muestraplanta.Seleccion(True, True)
	uo_muestraproductor.Seleccion(True, True)	
	//uo_muestrapredio.Seleccion(True, True)

	//Variedad
	uo_muestravariedad.cbx_todos.checked     = True
	uo_muestravariedad.cbx_consolida.checked = False
	uo_muestravariedad.cbx_consolida.visible = False
	uo_muestravariedad.dw_seleccion.enabled  = True
	uo_muestravariedad.filtra(21)
	
	//Zona
	uo_muestrazona.cbx_todos.checked     = True
	uo_muestrazona.cbx_consolida.checked = False
	uo_muestrazona.cbx_consolida.visible = False
	uo_muestrazona.dw_seleccion.enabled  = True
	
	//Planta
	uo_muestraplanta.cbx_todos.checked     = True
	uo_muestraplanta.cbx_consolida.checked = False
	uo_muestraplanta.cbx_consolida.visible = False
	uo_muestraplanta.dw_seleccion.enabled  = True
  //	uo_muestraplanta.filtra(1)	
	
	//Productor
	uo_muestraproductor.cbx_consolida.checked = False
	uo_muestraproductor.cbx_consolida.visible = False
	uo_muestraproductor.cbx_todos.checked     = True
	uo_muestraproductor.dw_seleccion.enabled  = True
	uo_muestraproductor.filtra(0)
	
   //Predio
//	uo_muestrapredio.cbx_todos.checked     = True
//	uo_muestrapredio.cbx_consolida.checked = False
//	uo_muestrapredio.cbx_consolida.visible = False
//	uo_muestrapredio.dw_seleccion.enabled  = True
// uo_muestrapredio.filtra(0)
	
	ddlb_Exporta.SelectItem(1)
	ddlb_Embarque.SelectItem(1)
		
	em_fechadesde.Enabled	=	True
	em_fechahasta.Enabled	=	True
	em_fechadesde.text	  	= String(Today())
	em_fechahasta.text	  	= String(Today())
	em_fechadesde.SetFocus()
	istr_Mant.Argumento[1] 	= '1'
	istr_Mant.Argumento[2] 	= '1'
	istr_Mant.Argumento[3] 	= '1'
	istr_Mant.Argumento[4] 	= '-1'
END IF

end event

type st_computador from w_para_informes`st_computador within w_inf_recepcion_tipo_embarque_ooo
end type

type st_usuario from w_para_informes`st_usuario within w_inf_recepcion_tipo_embarque_ooo
end type

type st_temporada from w_para_informes`st_temporada within w_inf_recepcion_tipo_embarque_ooo
end type

type p_logo from w_para_informes`p_logo within w_inf_recepcion_tipo_embarque_ooo
end type

type st_titulo from w_para_informes`st_titulo within w_inf_recepcion_tipo_embarque_ooo
boolean visible = false
integer x = 1984
integer y = 32
integer width = 114
boolean border = false
borderstyle borderstyle = stylebox!
end type

type pb_acepta from w_para_informes`pb_acepta within w_inf_recepcion_tipo_embarque_ooo
string tag = "Imprimir Reporte"
integer x = 1966
integer y = 220
integer taborder = 190
string powertiptext = "Imprimir Reporte"
end type

event pb_acepta::clicked;SetPointer(Arrow!)
Integer	li_fila, li_zona, li_variedad, li_consfec, li_planta, li_cliente, li_predio,&
         li_brix, li_firmeza, li_exporta, li_embarque, li_especie
Date		ld_FechaEmbaini, ld_FechaEmbafin
Long     ll_Productor

SetPointer(HourGlass!)

li_especie = ii_especie
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

//Figorífico
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

////Predio
//IF uo_muestrapredio.cbx_consolida.checked THEN
//	li_predio = -9
//ELSE
//   IF uo_muestrapredio.cbx_todos.checked THEN
	   li_predio = -1
//	  ELSE
//		  li_predio = uo_muestrapredio.dw_Seleccion.Object.codigo[1]
//		  IF IsNull(li_predio)THEN
//			  MessageBox("Atención","Debe Seleccionar un Predio Previamente",Exclamation!)
//			  RETURN
//		  END IF
//	  END IF
//  END IF

//Brix      
IF rb_tbrix.Checked THEN
	istr_Mant.Argumento[1] = '1' 
ELSEIF rb_mybrix.Checked THEN
	istr_Mant.Argumento[1] = '2'
ELSE
	istr_Mant.Argumento[1] = '3'
END IF

//Firmeza
IF rb_tfirmeza.Checked THEN
	istr_Mant.Argumento[2] = '1' 
ELSEIF rb_myfirmeza.Checked THEN
	istr_Mant.Argumento[2] = '2'
ELSE
	istr_Mant.Argumento[2] = '3'
END IF

//Exporta
IF IsNull(istr_Mant.Argumento[3])  THEN
	MessageBox("Atención","Debe Seleccionar Porcentaje Exportación Previamente",Exclamation!)
	RETURN
END IF

//Embarque
li_embarque	= Integer(istr_Mant.Argumento[4])
IF IsNull(li_embarque) OR li_embarque=0 THEN
	MessageBox("Atención","Debe Seleccionar Embarque Previamente",Exclamation!)
	RETURN
END IF		

//Fecha Recepción
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

IF li_especie = 21 THEN
   istr_info.titulo	= 'INFORME RESUMEN RECEPCION CEREZAS'
ELSEIF li_especie = 41 THEN
	istr_info.titulo	= 'INFORME RESUMEN RECEPCION KIWIS'
END IF

OpenWithParm(vinf,istr_info)

vinf.dw_1.DataObject = "dw_inf_recepcion_tipo_embarque"

vinf.dw_1.SetTransObject(sqlca)
li_fila = vinf.dw_1.Retrieve(li_variedad,li_zona,li_planta,ll_productor,li_predio,&
									  Integer(istr_Mant.Argumento[3]),ld_FechaEmbaini,ld_FechaEmbafin,&
									  Integer(istr_Mant.Argumento[1]),Integer(istr_Mant.Argumento[2]),li_embarque,li_especie)

IF li_fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ELSEIF li_fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", &
					StopSign!, Ok!)
ELSE
	F_Membrete(vinf.dw_1)
	vinf.dw_1.Modify("desde.text = '" + String(ld_FechaEmbaini,"dd/mm/yyyy") + "'")
	vinf.dw_1.Modify("hasta.text = '" + String(ld_FechaEmbafin,"dd/mm/yyyy") + "'")	
	vinf.dw_1.Object.DataWindow.Zoom = 98
	IF gs_Ambiente <> 'Windows' THEN
		F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)

	END IF
END IF

SetPointer(Arrow!)
end event

type pb_salir from w_para_informes`pb_salir within w_inf_recepcion_tipo_embarque_ooo
string tag = "Salir [Cerrar Ventana Activa]"
integer x = 1966
integer y = 500
integer taborder = 200
string powertiptext = "Salir [Cerrar Ventana Activa]"
end type

type em_fechadesde from editmask within w_inf_recepcion_tipo_embarque_ooo
integer x = 695
integer y = 1428
integer width = 338
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

type st_12 from statictext within w_inf_recepcion_tipo_embarque_ooo
integer x = 690
integer y = 1364
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
long backcolor = 12632256
boolean enabled = false
string text = "Desde"
boolean focusrectangle = false
end type

type st_13 from statictext within w_inf_recepcion_tipo_embarque_ooo
integer x = 1234
integer y = 1364
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

type em_fechahasta from editmask within w_inf_recepcion_tipo_embarque_ooo
integer x = 1234
integer y = 1532
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

type st_zona from statictext within w_inf_recepcion_tipo_embarque_ooo
integer x = 142
integer y = 296
integer width = 370
integer height = 84
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

type st_33 from statictext within w_inf_recepcion_tipo_embarque_ooo
integer x = 142
integer y = 528
integer width = 370
integer height = 84
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
boolean enabled = false
string text = "Productor"
boolean focusrectangle = false
end type

type cbx_todosfecha from checkbox within w_inf_recepcion_tipo_embarque_ooo
boolean visible = false
integer x = 2368
integer y = 1348
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
long backcolor = 12632256
string text = " "
end type

event clicked;//IF This.Checked THEN
//	em_fechadesde.Enabled	=	FALSE
//	em_fechahasta.Enabled	=	FALSE
//   istr_Mant.Argumento[12]	=	'01/01/1900'
//	istr_Mant.Argumento[13]	=	String(Today())
//	//cbx_consfecha.enabled   =  TRUE
//	cbx_consfecha.checked   =  FALSE
//ELSE
//	em_fechadesde.Enabled	=	TRUE
//	em_fechahasta.Enabled	=	TRUE
//	em_fechadesde.SetFocus()
//	//cbx_consfecha.enabled   =  FALSE
//END IF
//RETURN 0
//
//
//
end event

type st_14 from statictext within w_inf_recepcion_tipo_embarque_ooo
integer x = 142
integer y = 656
integer width = 370
integer height = 84
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
boolean enabled = false
string text = "Predio"
boolean focusrectangle = false
end type

type cbx_consfecha from checkbox within w_inf_recepcion_tipo_embarque_ooo
boolean visible = false
integer x = 2528
integer y = 1344
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
long backcolor = 12632256
string text = " "
end type

event clicked;//IF This.Checked THEN
//	em_fechadesde.Enabled	=	FALSE
//	em_fechahasta.Enabled	=	FALSE
//   istr_Mant.Argumento[12]	=	'01/01/1900'
//	istr_Mant.Argumento[13]	=	String(Today())
//	//cbx_consfecha.enabled   =  TRUE
//	cbx_todosfecha.Checked = False
//ELSE
//	em_fechadesde.Enabled	=	TRUE
//	em_fechahasta.Enabled	=	TRUE
//	em_fechadesde.SetFocus()
//	//cbx_consfecha.enabled   =  FALSE
//END IF
//RETURN 0
end event

type st_2 from statictext within w_inf_recepcion_tipo_embarque_ooo
integer x = 1573
integer y = 84
integer width = 197
integer height = 60
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

type uo_muestrazona from uo_seleccion_zonas_mod within w_inf_recepcion_tipo_embarque_ooo
integer x = 690
integer y = 256
integer width = 1088
integer height = 112
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
END CHOOSE
end event

type st_1 from statictext within w_inf_recepcion_tipo_embarque_ooo
integer x = 142
integer y = 412
integer width = 370
integer height = 72
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
string text = "Frigorífico"
boolean focusrectangle = false
end type

type st_4 from statictext within w_inf_recepcion_tipo_embarque_ooo
integer x = 137
integer y = 1432
integer width = 517
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
string text = "Fecha Recepción"
boolean focusrectangle = false
end type

type uo_muestravariedad from uo_seleccion_variedad_mod within w_inf_recepcion_tipo_embarque_ooo
integer x = 690
integer y = 136
integer width = 1088
integer height = 112
integer taborder = 160
boolean bringtotop = true
end type

on uo_muestravariedad.destroy
call uo_seleccion_variedad_mod::destroy
end on

type st_3 from statictext within w_inf_recepcion_tipo_embarque_ooo
integer x = 142
integer y = 172
integer width = 370
integer height = 64
boolean bringtotop = true
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

type uo_muestraproductor from uo_seleccion_productor_mod within w_inf_recepcion_tipo_embarque_ooo
event destroy ( )
integer x = 690
integer y = 496
integer width = 1088
integer height = 112
integer taborder = 140
boolean bringtotop = true
end type

on uo_muestraproductor.destroy
call uo_seleccion_productor_mod::destroy
end on

event dragdrop;call super::dragdrop;//IF IsNull(This.Codigo) THEN RETURN
//
//CHOOSE CASE This.Codigo
//	CASE -1, -9
//		uo_muestrapredio.Todos(True)
//			
//		uo_muestrapredio.cbx_Todos.Enabled	=	False
//				
//	CASE ELSE
//		uo_muestrapredio.Filtra(This.Codigo)
//					
//		uo_muestrapredio.cbx_Todos.Enabled	=	True
//		
//END CHOOSE
end event

type st_7 from statictext within w_inf_recepcion_tipo_embarque_ooo
integer x = 142
integer y = 656
integer width = 425
integer height = 84
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
string text = "% Exportación"
boolean focusrectangle = false
end type

type st_9 from statictext within w_inf_recepcion_tipo_embarque_ooo
integer x = 2427
integer y = 672
integer width = 402
integer height = 84
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
string text = "Embarque"
boolean focusrectangle = false
end type

type ddlb_exporta from dropdownlistbox within w_inf_recepcion_tipo_embarque_ooo
integer x = 690
integer y = 616
integer width = 882
integer height = 424
integer taborder = 180
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 16777215
boolean sorted = false
string item[] = {"Todos","Mayor a 75%,","De 50% a 75%","Menor a 50%"}
borderstyle borderstyle = stylelowered!
end type

event selectionchanged;IF Integer(index) = 1 THEN
	istr_Mant.Argumento[3] = String(1)
ELSEIF Integer(index) = 2 THEN
	istr_Mant.Argumento[3] = String(2)
ELSEIF Integer(index) = 3 THEN
	istr_Mant.Argumento[3] = String(3)
ELSEIF Integer(index) = 4 THEN
	istr_Mant.Argumento[3] = String(4)	
END IF
end event

type ddlb_embarque from dropdownlistbox within w_inf_recepcion_tipo_embarque_ooo
integer x = 2176
integer y = 836
integer width = 882
integer height = 400
integer taborder = 190
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 16777215
boolean sorted = false
string item[] = {"Todos","1","2","3"}
borderstyle borderstyle = stylelowered!
end type

event selectionchanged;IF Integer(index) = 1 THEN
	istr_Mant.Argumento[4] = String(-1)
ELSEIF Integer(index) = 2 THEN
	istr_Mant.Argumento[4] = String(1)
ELSEIF Integer(index) = 3 THEN
	istr_Mant.Argumento[4] = String(2)
ELSEIF Integer(index) = 4 THEN
	istr_Mant.Argumento[4] = String(3)	
END IF
end event

type gb_3 from groupbox within w_inf_recepcion_tipo_embarque_ooo
integer x = 73
integer y = 1312
integer width = 1737
integer height = 272
integer taborder = 200
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
borderstyle borderstyle = styleraised!
end type

type st_10 from statictext within w_inf_recepcion_tipo_embarque_ooo
integer x = 37
integer y = 1300
integer width = 1824
integer height = 324
integer textsize = -8
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

type uo_muestraplanta from uo_seleccion_planta_mod within w_inf_recepcion_tipo_embarque_ooo
event destroy ( )
integer x = 690
integer y = 376
integer width = 1088
integer height = 112
integer taborder = 140
boolean bringtotop = true
end type

on uo_muestraplanta.destroy
call uo_seleccion_planta_mod::destroy
end on

type gb_4 from groupbox within w_inf_recepcion_tipo_embarque_ooo
integer x = 73
integer y = 36
integer width = 1737
integer height = 764
integer taborder = 30
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
borderstyle borderstyle = styleraised!
end type

type uo_muestrapredio from uo_seleccion_predioproduc_mod within w_inf_recepcion_tipo_embarque_ooo
boolean visible = false
integer x = 695
integer y = 616
integer width = 1029
integer taborder = 210
boolean bringtotop = true
end type

on uo_muestrapredio.destroy
call uo_seleccion_predioproduc_mod::destroy
end on

type gb_5 from groupbox within w_inf_recepcion_tipo_embarque_ooo
integer x = 78
integer y = 800
integer width = 1737
integer height = 232
integer taborder = 180
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
string text = "Brix"
borderstyle borderstyle = styleraised!
end type

type rb_tbrix from radiobutton within w_inf_recepcion_tipo_embarque_ooo
integer x = 133
integer y = 896
integer width = 402
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
string text = "Todos"
boolean checked = true
end type

type rb_mybrix from radiobutton within w_inf_recepcion_tipo_embarque_ooo
integer x = 498
integer y = 896
integer width = 421
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
string text = "Mayor a 16º"
end type

type rb_mebrix from radiobutton within w_inf_recepcion_tipo_embarque_ooo
integer x = 1175
integer y = 896
integer width = 553
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
string text = "Menor a 16º"
end type

type gb_6 from groupbox within w_inf_recepcion_tipo_embarque_ooo
integer x = 78
integer y = 1040
integer width = 1737
integer height = 232
integer taborder = 180
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
string text = "Firmeza"
borderstyle borderstyle = styleraised!
end type

type st_44 from statictext within w_inf_recepcion_tipo_embarque_ooo
integer x = 37
integer y = 28
integer width = 1824
integer height = 1272
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

type rb_tfirmeza from radiobutton within w_inf_recepcion_tipo_embarque_ooo
integer x = 133
integer y = 1132
integer width = 402
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
string text = "Todos"
boolean checked = true
end type

type rb_myfirmeza from radiobutton within w_inf_recepcion_tipo_embarque_ooo
integer x = 498
integer y = 1132
integer width = 603
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
string text = "Mayor a 250g/cm2"
end type

type rb_mefirmeza from radiobutton within w_inf_recepcion_tipo_embarque_ooo
integer x = 1175
integer y = 1132
integer width = 608
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
string text = "Menor a 250g/cm2"
end type

