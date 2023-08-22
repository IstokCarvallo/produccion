$PBExportHeader$w_info_tiempo_enfr_cam.srw
forward
global type w_info_tiempo_enfr_cam from w_para_informes
end type
type dw_1 from datawindow within w_info_tiempo_enfr_cam
end type
end forward

global type w_info_tiempo_enfr_cam from w_para_informes
integer width = 2697
integer height = 992
string title = "Evaluacion de Enfriamiento y Temperatua"
dw_1 dw_1
end type
global w_info_tiempo_enfr_cam w_info_tiempo_enfr_cam

type variables


DataWindowChild	idwc_planta, idwc_tipoenvase, idwc_envase

uo_plantadesp			iuo_plantadesp

//String	is_NomPlanta
Integer	ii_Planta, ii_tipo_envase 
end variables

forward prototypes
public function boolean noexistetipoenvase (integer ai_tipo_enva)
public function boolean noexisteenvase (integer ai_envase, integer ai_tipo_enva)
end prototypes

public function boolean noexistetipoenvase (integer ai_tipo_enva);
String ls_tipo

SELECT	tien_nombre
	INTO	:ls_tipo
	FROM	dba.tiposenvases
	WHERE	enva_tipoen	=	:ai_tipo_enva ;
	
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de Tabla TiposEnvases")
	
	RETURN TRUE
ELSEIF sqlca.SQLCode = -1 THEN
	MessageBox("Atención", "Código de Tipo Envase (" + String(ai_tipo_enva) + &
					"), no ha sido creado en tabla respectiva.~r~r" + &
					"Ingrese o seleccione otro Código.")
	
	RETURN TRUE
END IF

RETURN FALSE


end function

public function boolean noexisteenvase (integer ai_envase, integer ai_tipo_enva);String ls_envase

SELECT	enva_nombre
	INTO	:ls_envase
	FROM	dba.envases
	WHERE	enva_codigo = 	:ai_envase AND
			enva_tipoen	=	:ai_tipo_enva;
	
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de Tabla Envases")
	
	RETURN TRUE
ELSEIF sqlca.SQLCode = -1 THEN
	MessageBox("Atención", "Código de Envase (" + String(ai_envase) + &
					"), no ha sido creado en tabla respectiva.~r~r" + &
					"Ingrese o seleccione otro Código.")
	
	RETURN TRUE
END IF

RETURN FALSE





end function

on w_info_tiempo_enfr_cam.create
int iCurrent
call super::create
this.dw_1=create dw_1
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_1
end on

on w_info_tiempo_enfr_cam.destroy
call super::destroy
destroy(this.dw_1)
end on

event open;x	=	0
y	=	0

iuo_plantadesp			=	Create uo_plantadesp

////Planta
dw_1.GetChild("planta", idwc_planta)
idwc_planta.SetTransObject(sqlca)
IF idwc_planta.Retrieve(gi_codexport) = 0 THEN
	MessageBox("Atención","Falta Registrar Plantas")
	idwc_planta.InsertRow(0)
END IF

//Tipos de Envase
dw_1.GetChild("tiposenvase",idwc_tipoenvase)
idwc_tipoenvase.SetTransObject(Sqlca)
IF idwc_tipoenvase.Retrieve() = 0 THEN
	MessageBox("Atención","Falta Registrar Tipos de Envase")
	idwc_tipoenvase.InsertRow(0)
END IF

//Envases
dw_1.GetChild("envase",idwc_envase)
idwc_envase.SetTransObject(Sqlca)
IF idwc_envase.Retrieve(0) = 0 THEN
	MessageBox("Atención","Falta Registrar Envases")
	idwc_envase.InsertRow(0)
END IF

dw_1.SetTransObject(SQLCA)
dw_1.InsertRow(0)

end event

type st_titulo from w_para_informes`st_titulo within w_info_tiempo_enfr_cam
integer x = 78
integer width = 2121
string text = "Informe de Enfriamiento y Temperatura"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_tiempo_enfr_cam
integer x = 2350
integer y = 300
integer taborder = 130
end type

event pb_acepta::clicked;
SetPointer(Arrow!)
 
Integer	li_tipoenvase, li_envase
Long		ll_Fila, ll_planta

istr_info.titulo	= 'CUENTA CORRIENTE ENVASES POR PLANTA'

OpenWithParm(vinf, istr_info)

vinf.dw_1.DataObject = "dw_info_enfriamiento_temp"

// Acepta Planta //
	IF dw_1.Object.consplanta[1]=1 THEN
		ll_planta = 90000
	END IF	

	ll_Planta = dw_1.Object.planta[1]
	IF IsNull(ll_Planta) Then
		MessageBox( "Planta Errónea", "Falta seleccionar una Planta.", &
	             StopSign!, Ok!)
		RETURN 1				 
   END IF

// Acepta Tipos de Envase //
IF dw_1.Object.todostiposenva[1] = 1 THEN
	 li_tipoenvase = 0
ELSE
	 li_tipoenvase = dw_1.Object.tiposenvase[1]
	If IsNull( li_tipoenvase) Then
		MessageBox( "Tipo de Envase Erróneo", "Falta seleccionar un Tipo de Envase.", &
	             StopSign!, Ok!)
		RETURN 1				 
   END If
END IF

// Acepta Envase //
IF dw_1.Object.todosenva[1] = 1 THEN
	li_envase = 0
ELSE
	 li_envase = dw_1.Object.envase[1]
	If IsNull( li_envase) Then
		MessageBox( "Envase Erróneo", "Falta seleccionar un Envase.", &
	             StopSign!, Ok!)
		RETURN 1				 
   END If
END IF

vinf.dw_1.SetTransObject(sqlca)

ll_Fila	=	vinf.dw_1.Retrieve(ll_Planta, li_TipoEnvase, li_Envase)

IF ll_Fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
			   	"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)

ELSEIF ll_Fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", &
	             StopSign!, Ok!)

ELSE
	   F_Membrete(vinf.dw_1)
		vinf.dw_1.Modify('DataWindow.Print.Preview = Yes')
		vinf.dw_1.Modify('DataWindow.Print.Preview.Zoom = 75')
		vinf.Visible	= True
		vinf.Enabled	= True
END IF

SetPointer(Arrow!)

end event

type pb_salir from w_para_informes`pb_salir within w_info_tiempo_enfr_cam
integer x = 2350
integer y = 592
integer taborder = 140
end type

type dw_1 from datawindow within w_info_tiempo_enfr_cam
integer x = 64
integer y = 240
integer width = 2162
integer height = 548
integer taborder = 140
boolean bringtotop = true
string title = "none"
string dataobject = "dw_info_tiempo_enfr_sel"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event itemchanged;Integer	li_Null
String	ls_Columna

SetNull(li_Null)

ls_Columna	=	dwo.Name

CHOOSE CASE ls_Columna
	CASE "planta"
		IF NOT iuo_PlantaDesp.Existe(Integer(data),True,Sqlca) THEN
			This.SetItem(1, "planta", li_Null )
			This.SetFocus()
			RETURN 1
		END IF		
							
	
	CASE "tiposenvase"
		
		IF noexistetipoenvase(Integer(data)) THEN
			This.SetItem(1, "tiposenvase", li_Null )
			This.Object.envase[row]	=	li_Null
         dw_1.Object.todosenva.protect = 1
			This.SetFocus()
			RETURN 1
		ELSE

			This.Object.envase[row]	=	li_Null
         dw_1.Object.todosenva.protect = 0
		   IF idwc_envase.Retrieve(Integer(data)) = 0 THEN
			   MessageBox("Atención","Falta Registrar Envases asociados a Tipos de Envases")
			   RETURN 1
		   END IF
		END IF

	CASE "envase"
		
		IF noexisteenvase(this.Object.tiposenvase[row],Integer(data)) THEN
			This.SetItem(1, "envase", li_Null )
			This.SetFocus()
			RETURN 1
		END IF
		
	
	CASE	"todostiposenva"
		IF	data = '1' THEN
			This.Object.tiposenvase[row]	=	li_Null
			dw_1.Object.envase[row]		= li_Null
			dw_1.Object.todosenva[row]	=	1
		END IF
		

	CASE	"todosenva"
		IF	data = '1' THEN This.Object.envase[row]	=	li_Null

	
END CHOOSE
end event

