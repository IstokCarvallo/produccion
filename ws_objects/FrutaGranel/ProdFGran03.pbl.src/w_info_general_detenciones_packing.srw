$PBExportHeader$w_info_general_detenciones_packing.srw
$PBExportComments$Informe Detenciones Packing
forward
global type w_info_general_detenciones_packing from w_para_informes
end type
type dw_1 from datawindow within w_info_general_detenciones_packing
end type
end forward

global type w_info_general_detenciones_packing from w_para_informes
integer width = 2505
integer height = 1224
string title = "Informe General Detenciones Packing"
dw_1 dw_1
end type
global w_info_general_detenciones_packing w_info_general_detenciones_packing

type variables


DataWindowChild	idwc_planta, idwc_especie, idwc_linea, idwc_exportador

uo_especie			iuo_especie
uo_lineapacking	iuo_lineapacking
uo_plantadesp		iuo_plantadesp
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

on w_info_general_detenciones_packing.create
int iCurrent
call super::create
this.dw_1=create dw_1
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_1
end on

on w_info_general_detenciones_packing.destroy
call super::destroy
destroy(this.dw_1)
end on

event open;call super::open;x	=	0
y	=	0

iuo_especie			=	Create uo_especie
iuo_lineapacking	=	Create uo_lineapacking
iuo_plantadesp    =  Create uo_plantadesp


dw_1.GetChild("exportador", idwc_exportador)
idwc_exportador.SetTransObject(sqlca)
IF idwc_exportador.Retrieve() = 0 THEN
	MessageBox("Atención","Falta Registrar Exportadores")
ELSE
	idwc_planta.InsertRow(0)
END IF

////Planta
dw_1.GetChild("planta", idwc_planta)
idwc_planta.SetTransObject(sqlca)
IF idwc_planta.Retrieve() = 0 THEN
	MessageBox("Atención","Falta Registrar Planta")
ELSE
	idwc_planta.InsertRow(0)
END IF

dw_1.SetItem(1,"planta",gstr_ParamPlanta.CodigoPlanta)

//especie
dw_1.GetChild("especie",idwc_especie)
idwc_especie.SetTransObject(Sqlca)
IF idwc_especie.Retrieve() = 0 THEN
	MessageBox("Atención","Falta Registrar Especie")
ELSE
	idwc_especie.SetTransObject(Sqlca)
	idwc_especie.InsertRow(0)
END IF

//Línea
dw_1.GetChild("linea",idwc_linea)
idwc_linea.SetTransObject(Sqlca)
IF idwc_linea.Retrieve(gstr_ParamPlanta.CodigoPlanta) = 0 THEN
	MessageBox("Atención","Falta Registrar Línea Packing")
ELSE
	idwc_linea.SetTransObject(Sqlca)
	idwc_linea.InsertRow(0)
END IF


dw_1.Object.fechad[1] = Date(string(today(),'dd/mm/yyyy'))


end event

event resize;call super::resize;st_titulo.x					=	160
st_titulo.y					=	284
end event

type pb_excel from w_para_informes`pb_excel within w_info_general_detenciones_packing
end type

type st_computador from w_para_informes`st_computador within w_info_general_detenciones_packing
integer x = 1682
end type

type st_usuario from w_para_informes`st_usuario within w_info_general_detenciones_packing
integer x = 1682
end type

type st_temporada from w_para_informes`st_temporada within w_info_general_detenciones_packing
integer x = 1682
end type

type p_logo from w_para_informes`p_logo within w_info_general_detenciones_packing
end type

type st_titulo from w_para_informes`st_titulo within w_info_general_detenciones_packing
integer x = 160
integer width = 1659
string text = "Informe General Detenciones Packing"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_general_detenciones_packing
integer x = 2025
integer y = 460
integer taborder = 50
end type

event pb_acepta::clicked;SetPointer(Arrow!)
 
Integer	li_Especie, li_linea, li_Area, li_Turno, li_planta
Long		ll_Fila
Date		ld_fechaCorte, ld_fechaprueba 

istr_info.titulo	= 'INFORME GENERAL DETENCIONES PACKING'
OpenWithParm(vinf, istr_info)
vinf.dw_1.DataObject = "dw_info_general_detencion_total"
dw_1.AcceptText()

// Acepta Planta //
li_Planta = dw_1.Object.planta[1]
IF IsNull(li_Planta) Then
	MessageBox( "Planta Errónea", "Falta seleccionar una Planta.", StopSign!, Ok!)
	RETURN 1				 
END IF

// Acepta Linea //
IF dw_1.Object.todaslinea[1] = 1 THEN
	li_Linea = 100
ELSE
   li_linea = dw_1.Object.linea[1]
	IF IsNull( li_linea) THEN
		MessageBox( "Línea Packing Erróneo", "Falta seleccionar una Línea Packing.", StopSign!, Ok!)
		RETURN 1				 
	END If
END IF	

//Acepta Fecha Corte//
ld_fechaCorte = dw_1.Object.fechad[1]

If IsNull(ld_fechaCorte) or ld_fechaCorte<= ld_fechaprueba Then
	MessageBox( "Fecha Erronea", "Se debe Ingresar una Fecha Corte.", StopSign!, Ok!)
	RETURN 1				 
END If

//Acepta Especie //
li_Especie = dw_1.Object.especie[1]
IF IsNull(li_Especie) Then
	MessageBox( "Especie Erróneo", "Falta seleccionar un Tipo de Especie.", StopSign!, Ok!)
	RETURN 1				 
END IF

//Acepta Turno//
li_Turno = dw_1.Object.turno[1]
IF IsNull(li_Turno) THEN
	MessageBox( "Turno Erróneo", "Falta seleccionar un Turno.", StopSign!, Ok!)
	RETURN 1				 
END If

vinf.dw_1.SetTransObject(sqlca)
ll_Fila	=	vinf.dw_1.Retrieve(li_planta, ld_FechaCorte,li_Especie,li_Linea)

IF ll_Fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
			   	"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ELSEIF ll_Fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", StopSign!, Ok!)
ELSE
	   F_Membrete(vinf.dw_1)
		vinf.dw_1.Modify('DataWindow.Print.Preview = Yes')
		vinf.dw_1.Modify('DataWindow.Print.Preview.Zoom = 100')
		vinf.Visible	= True
		vinf.Enabled	= True
END IF

SetPointer(Arrow!)


end event

type pb_salir from w_para_informes`pb_salir within w_info_general_detenciones_packing
integer x = 2025
integer y = 752
integer taborder = 60
end type

type dw_1 from datawindow within w_info_general_detenciones_packing
integer x = 160
integer y = 432
integer width = 1659
integer height = 516
integer taborder = 10
boolean bringtotop = true
string title = "none"
string dataobject = "dw_info_general_detenlineas_seleccion"
boolean border = false
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event itemchanged;Integer	li_Null,li_cliente
String	ls_Columna

SetNull(li_Null)

dw_1.accepttext()

li_cliente  =  dw_1.Object.exportador[1]

ls_Columna	=	dwo.Name

CHOOSE CASE ls_Columna
	CASE "planta"
		IF NOT iuo_PlantaDesp.Existe(Integer(data),True,Sqlca) THEN
			This.SetItem(1, "planta", li_Null )
			This.SetFocus()
			RETURN 1
		END IF		
							
	
	CASE "especie"
      IF NOT iuo_especie.Existe(Integer(data),True,SqlCa) THEN
			This.SetItem(1, "especie", li_Null)
			This.SetFocus()
			RETURN 1
		END IF
		

	CASE "linea"
		IF NOT iuo_lineapacking.Existe(gstr_ParamPlanta.CodigoPlanta,Integer(data),True,SqlCa) THEN
			This.SetItem(1, "linea", li_Null)
			This.SetFocus()
			RETURN 1
		END IF
		
	CASE "todaslinea"
		IF data="1" THEN
		  this.SetItem(1,"linea",li_null)
		END IF 

	
END CHOOSE
end event

