$PBExportHeader$w_info_ctacte_envases_planta.srw
$PBExportComments$Informe de Cuenta Corriente de Envases por Planta
forward
global type w_info_ctacte_envases_planta from w_para_informes
end type
type dw_1 from datawindow within w_info_ctacte_envases_planta
end type
end forward

global type w_info_ctacte_envases_planta from w_para_informes
integer width = 2725
integer height = 1204
string title = "Cuenta Corriente Envases por Planta"
dw_1 dw_1
end type
global w_info_ctacte_envases_planta w_info_ctacte_envases_planta

type variables
DataWindowChild	idwc_planta, idwc_tipoenvase, idwc_envase

uo_plantadesp			iuo_plantadesp

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

on w_info_ctacte_envases_planta.create
int iCurrent
call super::create
this.dw_1=create dw_1
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_1
end on

on w_info_ctacte_envases_planta.destroy
call super::destroy
destroy(this.dw_1)
end on

event open;call super::open;x	=	0
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
IF idwc_envase.Retrieve(-1) = 0 THEN
	MessageBox("Atención","Falta Registrar Envases")
	idwc_envase.InsertRow(0)
END IF

dw_1.SetTransObject(SQLCA)
dw_1.InsertRow(0)

end event

event resize;call super::resize;st_titulo.x					=	55
st_titulo.y					=	284
end event

type pb_excel from w_para_informes`pb_excel within w_info_ctacte_envases_planta
end type

type st_computador from w_para_informes`st_computador within w_info_ctacte_envases_planta
integer x = 1659
end type

type st_usuario from w_para_informes`st_usuario within w_info_ctacte_envases_planta
integer x = 1659
end type

type st_temporada from w_para_informes`st_temporada within w_info_ctacte_envases_planta
integer x = 1659
end type

type p_logo from w_para_informes`p_logo within w_info_ctacte_envases_planta
end type

type st_titulo from w_para_informes`st_titulo within w_info_ctacte_envases_planta
integer x = 55
integer width = 2162
string text = "Informe Saldo Envases por Planta"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_ctacte_envases_planta
integer x = 2290
integer y = 444
integer taborder = 130
end type

event pb_acepta::clicked;SetPointer(Arrow!)
 
Integer	li_tipoenvase, li_envase
Long		ll_Fila, ll_planta

istr_info.titulo	= 'CUENTA CORRIENTE ENVASES POR PLANTA'
OpenWithParm(vinf, istr_info)
vinf.dw_1.DataObject = "dw_info_ctacte_envase_planta"
dw_1.accepttext()

// Acepta Planta //
If dw_1.Object.todosplanta[1] = 1 Then
	ll_Planta = -1
	If dw_1.Object.consplanta[1]=1 Then ll_planta = -9
End If

// Acepta Tipos de Envase //
If dw_1.Object.todostiposenva[1] = 1 Then
	 li_tipoenvase = -1
Else
	 li_tipoenvase = dw_1.Object.tiposenvase[1]
End If

// Acepta Envase //
If dw_1.Object.todosenva[1] = 1 Then
	li_envase = -1
Else
	 li_envase = dw_1.Object.envase[1]
End If

vinf.dw_1.SetTransObject(sqlca)

ll_Fila	=	vinf.dw_1.Retrieve(ll_Planta, li_TipoEnvase, li_Envase)

If ll_Fila = -1 Then
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ElseIf ll_Fila = 0 Then
	MessageBox( "No Existe información", "No existe información para este informe.", StopSign!, Ok!)
Else
	   F_Membrete(vinf.dw_1)
		vinf.dw_1.ModIfy('DataWindow.Print.Preview = Yes')
		vinf.dw_1.ModIfy('DataWindow.Print.Preview.Zoom = 100')
		vinf.Visible	= True
		vinf.Enabled	= True
End If

SetPointer(Arrow!)

end event

type pb_salir from w_para_informes`pb_salir within w_info_ctacte_envases_planta
integer x = 2290
integer y = 736
integer taborder = 140
end type

type dw_1 from datawindow within w_info_ctacte_envases_planta
integer x = 101
integer y = 428
integer width = 2053
integer height = 444
integer taborder = 140
boolean bringtotop = true
string title = "none"
string dataobject = "dw_info_ctacte_envaplanta_seleccion"
boolean border = false
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event itemchanged;Integer	li_Null
String	ls_Columna

SetNull(li_Null)

ls_Columna	=	dwo.Name

CHOOSE CASE ls_Columna
	CASE "planta"
		If NOT iuo_PlantaDesp.Existe(Integer(data),True,Sqlca) Then
			This.SetItem(1, "planta", li_Null )
			This.SetFocus()
			RETURN 1
		End If		

	CASE "tiposenvase"		
		If noexistetipoenvase(Integer(data)) Then
			This.SetItem(1, "tiposenvase", li_Null )
			This.Object.envase[row]	=	li_Null
         	dw_1.Object.todosenva.protect = 1
			This.SetFocus()
			RETURN 1
		Else
			This.Object.envase[row]	=	li_Null
        	 	dw_1.Object.todosenva.protect = 0
		   	If idwc_envase.Retrieve(Integer(data)) = 0 Then
			   MessageBox("Atención","Falta Registrar Envases asociados a Tipos de Envases")
			   RETURN 1
		   End If
		End If

	CASE "envase"
		If noexisteenvase(this.Object.tiposenvase[row],Integer(data)) Then
			This.SetItem(1, "envase", li_Null )
			This.SetFocus()
			RETURN 1
		End If
		
	CASE "todosplanta"
		If data = '1' Then
			This.Object.planta[row]		=	li_Null
		End If
	
	CASE	"todostiposenva"
		If	data = '1' Then
			This.Object.tiposenvase[row]	=	li_Null
			dw_1.Object.envase[row]		= li_Null
			dw_1.Object.todosenva[row]	=	1
		End If
	
	CASE	"todosenva"
		If	data = '1' Then This.Object.envase[row]	=	li_Null

End CHOOSE
end event

