$PBExportHeader$w_mant_mues_ctlcalparammadurez.srw
$PBExportComments$Mantenedor Directo de Tipos de Inspección.
forward
global type w_mant_mues_ctlcalparammadurez from w_mant_directo
end type
type st_33 from statictext within w_mant_mues_ctlcalparammadurez
end type
type uo_muestraproductor from uo_seleccion_productor_mod within w_mant_mues_ctlcalparammadurez
end type
type st_zona from statictext within w_mant_mues_ctlcalparammadurez
end type
type uo_muestrazona from uo_seleccion_zonas_mod within w_mant_mues_ctlcalparammadurez
end type
type st_3 from statictext within w_mant_mues_ctlcalparammadurez
end type
type uo_muestravariedad from uo_seleccion_variedad_mod within w_mant_mues_ctlcalparammadurez
end type
type uo_muestraespecies from uo_seleccion_especie_mod within w_mant_mues_ctlcalparammadurez
end type
type st_1 from statictext within w_mant_mues_ctlcalparammadurez
end type
type uo_1 from uo_seleccion_prodpredio_mod within w_mant_mues_ctlcalparammadurez
end type
type st_14 from statictext within w_mant_mues_ctlcalparammadurez
end type
type pb_buscar from picturebutton within w_mant_mues_ctlcalparammadurez
end type
end forward

global type w_mant_mues_ctlcalparammadurez from w_mant_directo
integer width = 5038
integer height = 2132
string title = "PARAMETROS DE MADUREZ"
windowstate windowstate = maximized!
st_33 st_33
uo_muestraproductor uo_muestraproductor
st_zona st_zona
uo_muestrazona uo_muestrazona
st_3 st_3
uo_muestravariedad uo_muestravariedad
uo_muestraespecies uo_muestraespecies
st_1 st_1
uo_1 uo_1
st_14 st_14
pb_buscar pb_buscar
end type
global w_mant_mues_ctlcalparammadurez w_mant_mues_ctlcalparammadurez

type variables
DataWindowChild	idwc_cuartel

Integer				ii_tipo, ii_orden, ii_especie

uo_prodcuarteles	iuo_prodcuarteles
end variables

forward prototypes
public function boolean duplicado (string valor, integer tipo)
end prototypes

public function boolean duplicado (string valor, integer tipo);Long		ll_fila, ll_productor
Integer	li_zona, li_especie, li_variedad, li_predio, li_cuartel
Date		ld_fecha

ll_productor = uo_muestraproductor.codigo
li_especie	 = uo_muestraespecies.codigo
li_variedad	 = uo_muestravariedad.codigo
li_predio	 = uo_1.codigo
li_zona		 = uo_muestrazona.codigo	


IF tipo = 1 THEN
	
	ld_fecha = Date(valor)
	li_cuartel = dw_1.Object.prcc_codigo[il_fila]
	
ElSE	
	
	li_cuartel = Integer(valor)
	ld_fecha = dw_1.Object.pama_fecmue[il_fila]

END IF	

ll_fila	= dw_1.Find ("prod_codigo = " + String(ll_productor) + &
								" AND espe_codigo = " + String(li_especie) + &
								" AND vari_codigo = " + String(li_variedad) + &
								" AND prpr_codigo = " + String(li_predio)+ &								
								" AND prcc_codigo = " + String(li_cuartel)+ &								
								" AND String(pama_fecmue) = '" + String(ld_fecha) + "'", 1, dw_1.RowCount())	

IF ll_fila > 0  THEN
	MessageBox("Error","Registro ya fue ingresado anteriormente",Information!, Ok!)
	RETURN True
ELSE
	RETURN False
END IF



end function

event ue_recuperadatos;Long	ll_fila, respuesta

DO	
	
	ll_fila	= dw_1.Retrieve(uo_muestraproductor.codigo,ii_especie,&
						uo_muestravariedad.codigo,uo_1.codigo,uo_muestrazona.codigo	)

	IF ll_fila = -1 THEN
		respuesta = MessageBox(	"Error en Base de Datos", "No es posible conectar la Base de Datos.", &
										Information!, RetryCancel!)
	ELSEIF ll_fila > 0 THEN
		dw_1.GetChild("prcc_codigo", idwc_cuartel)
		idwc_cuartel.SetTransObject(sqlca)
		idwc_cuartel.Retrieve(uo_muestraproductor.codigo,uo_1.codigo)
		dw_1.SetFocus()
		il_fila					= 1
		pb_imprimir.Enabled	= True
		pb_insertar.Enabled	= True
		pb_eliminar.Enabled	= True
		pb_grabar.Enabled		= True
		uo_muestrazona.dw_seleccion.Enabled = False
		uo_muestraespecies.dw_seleccion.Enabled = False
		uo_muestravariedad.dw_seleccion.Enabled = False
		uo_muestraproductor.dw_seleccion.Enabled = False
		uo_1.dw_seleccion.Enabled = False
	ELSE
		pb_insertar.Enabled	= True
		pb_insertar.SetFocus()
	END IF

LOOP WHILE respuesta = 1

IF respuesta = 2 THEN Close(This)
end event

event open;call super::open;String ls_especie
x = 0
y = 0
This.Icon	=	Gstr_apl.Icono

Boolean	lb_Cerrar

ls_especie	= Message.StringParm
ii_especie = Integer(ls_especie)

IF IsNull(uo_muestrazona.Codigo) THEN lb_Cerrar	=	True
IF IsNull(uo_muestraespecies.Codigo) THEN lb_Cerrar	=	True
IF IsNull(uo_muestravariedad.Codigo) THEN lb_Cerrar	=	True
IF IsNull(uo_muestraproductor.Codigo) THEN lb_Cerrar	=	True
IF IsNull(uo_1.Codigo) THEN lb_Cerrar	=	True

IF lb_Cerrar THEN
	Close(This)
ELSE
	uo_muestraespecies.Seleccion(True, True)
	uo_muestravariedad.Seleccion(True, True)
	uo_muestrazona.Seleccion(True, True)
	uo_muestraproductor.Seleccion(True, True)	
  	uo_1.Seleccion(True, True)	
	
	//Especie
	uo_muestraespecies.cbx_todos.checked     = FALSE
	uo_muestraespecies.cbx_todos.visible     = FALSE
	uo_muestraespecies.cbx_consolida.visible = FALSE
	uo_muestraespecies.dw_seleccion.enabled  = TRUE
	uo_muestraespecies.dw_seleccion.object.codigo[1] = ii_especie 
	uo_muestravariedad.Filtra(ii_especie)
	uo_muestraespecies.dw_seleccion.enabled  = TRUE
	
	//Variedad
	uo_muestravariedad.cbx_todos.checked     	= false
	uo_muestravariedad.cbx_consolida.checked 	= FALSE
	uo_muestravariedad.cbx_todos.visible 		= FALSE
	uo_muestravariedad.dw_seleccion.enabled  	= TRUE
	
	//Zona
	uo_muestrazona.cbx_todos.checked     = False
	uo_muestrazona.cbx_consolida.checked = FALSE
	uo_muestrazona.cbx_todos.checked     = False
	uo_muestrazona.cbx_consolida.Visible = FALSE
	uo_muestrazona.dw_seleccion.Visible  = True
		
	//Productor
	uo_muestraproductor.cbx_consolida.checked = FALSE
	uo_muestraproductor.cbx_todos.checked     = False
	uo_muestraproductor.dw_seleccion.enabled  = TRUE
	uo_muestraproductor.cbx_consolida.Visible = FALSE
	uo_muestraproductor.cbx_todos.Visible      = False
	uo_muestraproductor.filtra(0)
	
	//Predio
	uo_1.cbx_todos.checked     = FALSE
	uo_1.cbx_consolida.checked = False
	uo_1.dw_seleccion.enabled  = TRUE
	uo_1.cbx_todos.Visible     = FALSE
	uo_1.cbx_consolida.Visible = False
	
	dw_1.GetChild("prcc_codigo", idwc_cuartel)
	idwc_cuartel.SetTransObject(sqlca)
	idwc_cuartel.Retrieve(0,0)
	
	uo_muestrazona.SetFocus()	
	
	iuo_prodcuarteles = Create uo_prodcuarteles
END IF
end event

on w_mant_mues_ctlcalparammadurez.create
int iCurrent
call super::create
this.st_33=create st_33
this.uo_muestraproductor=create uo_muestraproductor
this.st_zona=create st_zona
this.uo_muestrazona=create uo_muestrazona
this.st_3=create st_3
this.uo_muestravariedad=create uo_muestravariedad
this.uo_muestraespecies=create uo_muestraespecies
this.st_1=create st_1
this.uo_1=create uo_1
this.st_14=create st_14
this.pb_buscar=create pb_buscar
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_33
this.Control[iCurrent+2]=this.uo_muestraproductor
this.Control[iCurrent+3]=this.st_zona
this.Control[iCurrent+4]=this.uo_muestrazona
this.Control[iCurrent+5]=this.st_3
this.Control[iCurrent+6]=this.uo_muestravariedad
this.Control[iCurrent+7]=this.uo_muestraespecies
this.Control[iCurrent+8]=this.st_1
this.Control[iCurrent+9]=this.uo_1
this.Control[iCurrent+10]=this.st_14
this.Control[iCurrent+11]=this.pb_buscar
end on

on w_mant_mues_ctlcalparammadurez.destroy
call super::destroy
destroy(this.st_33)
destroy(this.uo_muestraproductor)
destroy(this.st_zona)
destroy(this.uo_muestrazona)
destroy(this.st_3)
destroy(this.uo_muestravariedad)
destroy(this.uo_muestraespecies)
destroy(this.st_1)
destroy(this.uo_1)
destroy(this.st_14)
destroy(this.pb_buscar)
end on

event ue_validaregistro();call super::ue_validaregistro;//Integer	li_cont
//String	ls_mensaje, ls_colu[]
//
//IF Isnull(dw_1.Object.ccti_codigo[il_fila]) OR dw_1.Object.ccti_codigo[il_fila] = 0 THEN
//	li_cont ++
//	ls_mensaje 			= ls_mensaje + "~nTipo de Inspección"
//	ls_colu[li_cont]	= "ccti_codigo"
//END IF
//
//IF Isnull(dw_1.Object.ccti_descrip[il_fila]) OR dw_1.Object.ccti_descrip[il_fila] = "" THEN
//	li_cont ++
//	ls_mensaje 			= ls_mensaje + "~nNombre de Inspección"
//	ls_colu[li_cont]	= "ccti_descrip"
//END IF
//
//
//IF li_cont > 0 THEN
//	MessageBox("Error de Consistencia", "Falta el ingreso de :" + ls_mensaje + ".", StopSign!, Ok!)
//	dw_1.SetColumn(ls_colu[1])
//	dw_1.SetFocus()
//	Message.DoubleParm = -1
//END IF
end event

event ue_imprimir;SetPointer(HourGlass!)

Integer	li_zona
Long		fila
str_info	lstr_info

lstr_info.titulo	= "PARAMETROS DE MADUREZ"
lstr_info.copias	= 1

OpenWithParm(vinf,lstr_info)

vinf.dw_1.DataObject = "dw_info_parammadurez"
vinf.dw_1.SetTransObject(sqlca)

fila = vinf.dw_1.Retrieve(uo_muestraproductor.codigo,ii_especie,&
						uo_muestravariedad.codigo,-1,uo_1.codigo,uo_muestrazona.codigo)

IF fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ELSEIF fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", &
					StopSign!, Ok!)
ELSE
	F_Membrete(vinf.dw_1)
	vinf.dw_1.Sort()
	IF gs_Ambiente <> 'Windows' THEN
		F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)

	END IF
END IF

SetPointer(Arrow!)
end event

event ue_guardar;
SetPointer(HourGlass!)

w_main.SetMicroHelp("Grabando información...")

Message.DoubleParm = 0

TriggerEvent("ue_antesguardar")

IF Message.DoubleParm = -1 THEN RETURN

IF wf_actualiza_db() THEN
	w_main.SetMicroHelp("Información Grabada.")
	pb_imprimir.Enabled	= True
ELSE
	w_main.SetMicroHelp("No se puede Grabar información.")
	Message.DoubleParm = -1
	RETURN
END IF
end event

event ue_antesguardar;call super::ue_antesguardar;Integer	li_cont
String	ls_mensaje, ls_colu[]
Long		ll_fila

uo_muestraespecies.codigo = ii_especie

FOR ll_fila = 1 TO dw_1.RowCount()
	dw_1.Object.zona_codigo[ll_fila] = uo_muestrazona.codigo 
	dw_1.Object.prod_codigo[ll_fila] = uo_muestraproductor.codigo
	dw_1.object.espe_codigo[ll_fila] = ii_especie 
	dw_1.object.vari_codigo[ll_fila] = uo_muestravariedad.codigo
	dw_1.object.prpr_codigo[ll_fila] = uo_1.codigo
NEXT	

IF Isnull(dw_1.Object.zona_codigo[il_fila]) OR dw_1.Object.zona_codigo[il_fila] = 0 THEN
	li_cont ++
	ls_mensaje 			= ls_mensaje + "~nZona"
	ls_colu[li_cont]	= "zona_codigo"
END IF

IF Isnull(dw_1.Object.espe_codigo[il_fila]) OR dw_1.Object.espe_codigo[il_fila] = 0 THEN
	li_cont ++
	ls_mensaje 			= ls_mensaje + "~nEspecie"
	ls_colu[li_cont]	= "espe_codigo"
END IF

IF Isnull(dw_1.Object.vari_codigo[il_fila]) OR dw_1.Object.vari_codigo[il_fila] = 0 THEN
	li_cont ++
	ls_mensaje 			= ls_mensaje + "~nVariedad"
	ls_colu[li_cont]	= "vari_codigo"
END IF

IF Isnull(dw_1.Object.prod_codigo[il_fila]) OR dw_1.Object.prod_codigo[il_fila] = 0 THEN
	li_cont ++
	ls_mensaje 			= ls_mensaje + "~nProductor"
	ls_colu[li_cont]	= "prod_codigo"
END IF

IF Isnull(dw_1.Object.insp_codigo[il_fila]) OR dw_1.Object.insp_codigo[il_fila] = 0 THEN
	li_cont ++
	ls_mensaje 			= ls_mensaje + "~nInspector"
	ls_colu[li_cont]	= "insp_codigo"
END IF

IF Isnull(dw_1.Object.prpr_codigo[il_fila]) OR dw_1.Object.prpr_codigo[il_fila] = 0 THEN
	li_cont ++
	ls_mensaje 			= ls_mensaje + "~nPredio"
	ls_colu[li_cont]	= "prpr_codigo"
END IF

IF li_cont > 0 THEN
	MessageBox("Error de Consistencia", "Falta el ingreso de :" + ls_mensaje + ".", StopSign!, Ok!)
	dw_1.SetColumn(ls_colu[1])
	dw_1.SetFocus()
	Message.DoubleParm = -1
END IF
end event

event ue_nuevo;call super::ue_nuevo;dw_1.object.pama_fecmue[il_fila] = Date(Now())
end event

type st_encabe from w_mant_directo`st_encabe within w_mant_mues_ctlcalparammadurez
integer x = 951
integer y = 24
integer width = 2999
integer height = 388
boolean enabled = true
end type

type pb_nuevo from w_mant_directo`pb_nuevo within w_mant_mues_ctlcalparammadurez
integer x = 4731
integer y = 448
integer taborder = 60
end type

event pb_nuevo::clicked;IF Not istr_mant.Solo_Consulta THEN
	CHOOSE CASE wf_modifica()
		CASE 0
			CHOOSE CASE MessageBox("Grabar registro(s)","Desea Grabar la información ?", Question!, YesNoCancel!)
				CASE 1
					Message.DoubleParm = 0
					Parent.TriggerEvent("ue_guardar")
					IF message.DoubleParm = -1 THEN RETURN
					
				CASE 3
					RETURN
			END CHOOSE
	END CHOOSE
END IF

dw_1.Reset()

pb_insertar.Enabled	= False
pb_eliminar.Enabled	= False
pb_grabar.Enabled		= False
pb_imprimir.Enabled	= False
pb_lectura.Enabled	= True
il_fila					= 0

uo_muestrazona.dw_seleccion.Enabled = True
uo_muestraespecies.dw_seleccion.Enabled = False
uo_muestravariedad.dw_seleccion.Enabled = True
uo_muestraproductor.dw_seleccion.Enabled = True
uo_1.dw_seleccion.Enabled = True
end event

type pb_lectura from w_mant_directo`pb_lectura within w_mant_mues_ctlcalparammadurez
integer x = 4722
integer y = 152
integer taborder = 50
end type

event pb_lectura::clicked;IF 	NOT isnull(uo_muestrazona.codigo) OR uo_muestrazona.codigo <> -1 OR &
	NOT isnull(uo_muestraespecies.codigo) OR uo_muestraespecies.codigo <> -1 OR &
	NOT isnull(uo_muestravariedad.codigo) OR uo_muestravariedad.codigo <> -1 OR &
	NOT isnull(uo_muestraproductor.codigo) OR uo_muestraproductor.codigo <> -1 OR &
	NOT isnull(uo_1.codigo) OR uo_1.codigo <> -1 THEN
				
	Parent.PostEvent("ue_recuperadatos")
ELSE
	MessageBox("Error","Falta seleccionar datos en el encabezado",Information!, Ok!)
	Return
					
END IF	
//pb_lectura.Enabled = FALSE
end event

type pb_eliminar from w_mant_directo`pb_eliminar within w_mant_mues_ctlcalparammadurez
integer x = 4722
integer y = 808
integer taborder = 80
end type

type pb_insertar from w_mant_directo`pb_insertar within w_mant_mues_ctlcalparammadurez
integer x = 4722
integer y = 628
integer taborder = 70
end type

forward prototypes
public function integer duplicadodatos ()
end prototypes

public function integer duplicadodatos ();return 1
end function

type pb_salir from w_mant_directo`pb_salir within w_mant_mues_ctlcalparammadurez
integer x = 4722
integer y = 1552
integer taborder = 110
end type

type pb_imprimir from w_mant_directo`pb_imprimir within w_mant_mues_ctlcalparammadurez
integer x = 4722
integer y = 1168
integer taborder = 100
end type

type pb_grabar from w_mant_directo`pb_grabar within w_mant_mues_ctlcalparammadurez
integer x = 4722
integer y = 988
integer taborder = 90
end type

type dw_1 from w_mant_directo`dw_1 within w_mant_mues_ctlcalparammadurez
integer x = 46
integer y = 420
integer width = 4521
integer height = 1588
integer taborder = 120
string dataobject = "dw_mues_ctlcalparammadurez"
boolean hscrollbar = true
end type

event dw_1::itemchanged;Long		ll_null 

SetNull(ll_null)

CHOOSE CASE dwo.Name
	
	CASE "pama_fecmue"
		IF Duplicado(data,1) THEN
			This.SetItem(row, "pama_fecmue", Date(NOW()))
			RETURN 1			
		END IF 
				
	CASE "prcc_codigo"	
		IF Duplicado(data,2) THEN
			This.SetItem(row, "prcc_codigo", Integer(ll_null))
			RETURN 1	
		ELSEIF NOT iuo_prodcuarteles.Existe(uo_muestraproductor.codigo,uo_1.codigo	,Integer(Data),True,SqlCa) THEN
			This.SetItem(row, "prcc_codigo", Integer(ll_null))
			RETURN 1	
		END IF 
		
		
	CASE "pama_colpre"
		IF Integer(Data) > Integer(10) THEN  
			MessageBox("Error","Color no puede ser mayor a 10",Information!, Ok!)
			This.SetItem(row, "pama_colpre", Integer(ll_null))
			RETURN 1	
		END IF	
END CHOOSE


IF dw_1.RowCount() > 0 THEN
	pb_grabar.Enabled = True
	pb_eliminar.Enabled = True
END IF	
end event

event dw_1::rowfocuschanged;call super::rowfocuschanged;////IF CurrentRow > 0 AND il_fila > 0 THEN
////	ias_campo[1] = String(dw_1.Object.cctc_codigo[il_fila])
////	ias_campo[2] = dw_1.Object.cctc_nombres[il_fila]
////	ias_campo[3] = dw_1.Object.cctc_abrevi[il_fila]
////END IF
//
//Integer li_Fila
//
//li_Fila = dw_1.RowCount()
//
//IF RowCount() < 1 OR GetRow() = 0 OR ib_borrar THEN
//	ib_datos_ok = False
//ELSE
//	il_fila				=	GetRow()
//	pb_grabar.Enabled	=	True
//END IF
//
end event

event dw_1::itemfocuschanged;call super::itemfocuschanged;//CHOOSE CASE dwo.Name
//
//	CASE "ccin_abrevi"
//			pb_grabar.Enabled	=	True
//
////	CASE "cctc_codigo"
////			TriggerEvent("ue_validaregistro")
//
//END CHOOSE
//
end event

event dw_1::sqlpreview;//										
end event

type st_33 from statictext within w_mant_mues_ctlcalparammadurez
integer x = 2331
integer y = 188
integer width = 370
integer height = 84
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 553648127
boolean enabled = false
string text = "Productor"
boolean focusrectangle = false
end type

type uo_muestraproductor from uo_seleccion_productor_mod within w_mant_mues_ctlcalparammadurez
event destroy ( )
integer x = 2661
integer y = 152
integer width = 887
integer height = 124
integer taborder = 30
boolean bringtotop = true
end type

on uo_muestraproductor.destroy
call uo_seleccion_productor_mod::destroy
end on

event ue_cambio;call super::ue_cambio;IF IsNull(This.Codigo) THEN RETURN

CHOOSE CASE This.Codigo
	CASE -1, -9
		uo_1.Todos(True)
			
		uo_1.cbx_Todos.Enabled	=	False
				
	CASE ELSE
		uo_1.Filtra(This.Codigo)
					
		uo_1.Productor	=	This.Codigo

		uo_1.cbx_Todos.Enabled	=	True
		
END CHOOSE
end event

type st_zona from statictext within w_mant_mues_ctlcalparammadurez
integer x = 1111
integer y = 188
integer width = 297
integer height = 84
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 553648127
boolean enabled = false
string text = "Zona"
boolean focusrectangle = false
end type

type uo_muestrazona from uo_seleccion_zonas_mod within w_mant_mues_ctlcalparammadurez
integer x = 1403
integer y = 152
integer width = 887
integer height = 124
integer taborder = 10
boolean bringtotop = true
end type

on uo_muestrazona.destroy
call uo_seleccion_zonas_mod::destroy
end on

type st_3 from statictext within w_mant_mues_ctlcalparammadurez
integer x = 1106
integer y = 292
integer width = 297
integer height = 84
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 553648127
string text = "Variedad"
boolean focusrectangle = false
end type

type uo_muestravariedad from uo_seleccion_variedad_mod within w_mant_mues_ctlcalparammadurez
integer x = 1403
integer y = 260
integer width = 887
integer height = 124
integer taborder = 20
boolean bringtotop = true
end type

on uo_muestravariedad.destroy
call uo_seleccion_variedad_mod::destroy
end on

type uo_muestraespecies from uo_seleccion_especie_mod within w_mant_mues_ctlcalparammadurez
event destroy ( )
integer x = 1403
integer y = 44
integer width = 887
integer height = 124
boolean bringtotop = true
boolean enabled = false
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

type st_1 from statictext within w_mant_mues_ctlcalparammadurez
integer x = 2331
integer y = 292
integer width = 306
integer height = 84
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 553648127
string text = "Predio"
boolean focusrectangle = false
end type

type uo_1 from uo_seleccion_prodpredio_mod within w_mant_mues_ctlcalparammadurez
integer x = 2661
integer y = 256
integer width = 910
integer height = 124
integer taborder = 40
boolean bringtotop = true
end type

on uo_1.destroy
call uo_seleccion_prodpredio_mod::destroy
end on

event ue_cambio;call super::ue_cambio;dw_1.GetChild("prcc_codigo", idwc_cuartel)
idwc_cuartel.SetTransObject(sqlca)
idwc_cuartel.Retrieve(This.Codigo,uo_muestraproductor.codigo)
end event

type st_14 from statictext within w_mant_mues_ctlcalparammadurez
integer x = 1111
integer y = 80
integer width = 297
integer height = 84
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 553648127
boolean enabled = false
string text = "Especie"
boolean focusrectangle = false
end type

type pb_buscar from picturebutton within w_mant_mues_ctlcalparammadurez
integer x = 3662
integer y = 212
integer width = 155
integer height = 132
integer taborder = 30
boolean bringtotop = true
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string picturename = "\Desarrollo\Bmp\BUSCAE.BMP"
string disabledname = "\Desarrollo\Bmp\BUSCAD.BMP"
alignment htextalign = left!
end type

event clicked;str_busqueda	lstr_busq

lstr_busq.Argum[1]	=	String(ii_especie)

OpenWithParm(w_busc_parametrosmadurez,lstr_busq)

lstr_busq	= Message.PowerObjectParm

IF lstr_busq.argum[2] <> "" THEN
	uo_1.dw_seleccion.object.codigo[1] 						=	Integer(lstr_busq.argum[6])
	uo_muestravariedad.dw_seleccion.object.codigo[1] 	= 	Integer(lstr_busq.argum[3])
	uo_muestrazona.dw_seleccion.object.codigo[1] 		= 	Integer(lstr_busq.argum[2])
	uo_muestraproductor.dw_seleccion.object.codigo[1]	= 	Integer(lstr_busq.argum[4])
	uo_muestraproductor.codigo								=	Integer(lstr_busq.argum[4])
	uo_muestravariedad.codigo									=	Integer(lstr_busq.argum[3])
	uo_1.codigo														=	Integer(lstr_busq.argum[6])
	uo_muestrazona.codigo										=	Integer(lstr_busq.argum[2])
	Parent.PostEvent("ue_recuperadatos")
ELSE
	pb_buscar.SetFocus()
END IF
end event

