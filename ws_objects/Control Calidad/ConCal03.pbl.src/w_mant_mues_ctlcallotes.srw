$PBExportHeader$w_mant_mues_ctlcallotes.srw
$PBExportComments$ventana de muestra de la tabla CTLCALLOTES
forward
global type w_mant_mues_ctlcallotes from w_mant_tabla
end type
type st_1 from statictext within w_mant_mues_ctlcallotes
end type
type uo_selplantas from uo_seleccion_plantas within w_mant_mues_ctlcallotes
end type
end forward

global type w_mant_mues_ctlcallotes from w_mant_tabla
string tag = "Mantenedor de Tabla CTLCALLOTES"
integer width = 5175
string title = "MAESTRO LOTES"
st_1 st_1
uo_selplantas uo_selplantas
end type
global w_mant_mues_ctlcallotes w_mant_mues_ctlcallotes

type variables
w_mant_deta_ctlcallotes  iw_mantencion

DataWindowChild	 	idwc_productor,idwc_especie, idwc_variedad, idwc_embalaje,&
                 			    idwc_calibre, idwc_packing, idwc_planta



end variables

forward prototypes
public function boolean noexisteplanta (integer as_planta)
end prototypes

public function boolean noexisteplanta (integer as_planta);String as_nombre_planta

SELECT	plde_nombre
	INTO	:as_nombre_planta
	FROM	dbo.PLANTADESP
	WHERE	plde_codigo	=	:as_planta ;
	
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de tabla PLANTADESP")
	
	RETURN True
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención", "Código de Planta no ha sido Definido.~r~r" + &
					"Ingrese o seleccione otro código.")
	
	RETURN True
END IF	

RETURN False
end function

on w_mant_mues_ctlcallotes.create
int iCurrent
call super::create
this.st_1=create st_1
this.uo_selplantas=create uo_selplantas
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_1
this.Control[iCurrent+2]=this.uo_selplantas
end on

on w_mant_mues_ctlcallotes.destroy
call super::destroy
destroy(this.st_1)
destroy(this.uo_selplantas)
end on

event open;call super::open;Boolean	lb_Cerrar = False

If IsNull(uo_SelPlantas.Codigo) Then lb_Cerrar = True

If lb_Cerrar Then
	Close(This)
Else
	uo_SelPlantas.Seleccion(False, False)
	uo_SelPlantas.Filtra(1)
	uo_SelPlantas.Inicia(gi_CodPlanta)
	
	buscar	=	"Numero:Ncclo_numero,Productor:Nprod_codigo,Especie:Nespe_codigo"
	ordenar	=	"Numero:cclo_numero,Productor:prod_codigo,Especie:espe_codigo"
End If
end event

event ue_recuperadatos;Long	ll_fila, respuesta
Integer  li_grupo
String   ls_Usuario

ls_Usuario	=	Upper(Gstr_Us.Nombre)

DO
	dw_1.GetChild("espe_codigo", idwc_especie)
	idwc_especie.SetTransObject(SQLCA)
	idwc_especie.Retrieve()
	
	dw_1.GetChild("emba_codigo", idwc_embalaje)
	idwc_embalaje.SetTransObject(SQLCA)
	idwc_embalaje.Retrieve()
	
	dw_1.GetChild("plde_codpak", idwc_packing)
	idwc_packing.SetTransObject(SQLCA)
	idwc_packing.Retrieve()
			
	ll_fila			=	dw_1.Retrieve(gi_CodExport, uo_SelPlantas.Codigo)
		
	IF ll_fila 		=	-1 THEN
		respuesta 	=	MessageBox("Error en Base de Datos", "No es posible conectar la Base de Datos.", Information!, RetryCancel!)
	ELSEIF ll_fila > 0 THEN
		
		IF dw_1.rowcount() > 0 THEN
		  li_grupo = BUSCAGRUPO(ls_Usuario)
		  IF  li_Grupo	<=  2 THEN 
				dw_1.SetFocus()
				il_fila					= 1
				pb_imprimir.Enabled	= True
				pb_insertar.Enabled	= True
				pb_eliminar.Enabled	= True
				pb_grabar.Enabled		= True	
			ELSE
				pb_insertar.SetFocus()	
				pb_insertar.Enabled	= False
				pb_imprimir.Enabled	= True	
			END IF 
		END IF 
	END IF


LOOP WHILE respuesta = 1

IF respuesta = 2 THEN Close(This)
end event

event ue_borrar;IF dw_1.rowcount() < 1 THEN RETURN

SetPointer(HourGlass!)

ib_borrar = True
w_main.SetMicroHelp("Validando la eliminación...")

Message.DoubleParm = 0

This.TriggerEvent ("ue_validaborrar")

IF Message.DoubleParm = -1 THEN RETURN

istr_mant.borra	= True
istr_mant.agrega	= False

istr_mant.argumento[1]	=	String(gi_CodExport)
istr_mant.argumento[2]	=	String(uo_SelPlantas.Codigo)

OpenWithParm(iw_mantencion, istr_mant) 

istr_mant = Message.PowerObjectParm

IF istr_mant.respuesta = 1 THEN
	IF dw_1.DeleteRow(0) = 1 THEN
		ib_borrar = False
		w_main.SetMicroHelp("Borrando Registro...")
		SetPointer(Arrow!)
	ELSE
		ib_borrar = False
		MessageBox(This.Title,"No se puede borrar actual registro.")
	END IF
	
	IF dw_1.RowCount() = 0 THEN
		pb_eliminar.Enabled = False
	ELSE
		il_fila = dw_1.GetRow()
		dw_1.SelectRow(il_fila,True)
	END IF
END IF

istr_mant.borra	 = False
end event

event ue_imprimir;Long		fila
str_info	lstr_info

lstr_info.titulo	= "LOTES"
lstr_info.copias	= 1

OpenWithParm(vinf,lstr_info)
vinf.dw_1.DataObject = "dw_info_ctlcallotes"
vinf.dw_1.SetTransObject(sqlca)
fila = vinf.dw_1.Retrieve(gi_CodExport, uo_SelPlantas.Codigo)

If fila = -1 Then
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ElseIf fila = 0 Then
	MessageBox( "No Existe información", "No existe información para este informe.", StopSign!, Ok!)
Else
	F_Membrete(vinf.dw_1)
//	vinf.dw_1.Modify('DataWindow.Zoom = 95')
	If gs_Ambiente <> 'Windows' Then F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
End If
end event

event ue_modifica;call super::ue_modifica;istr_mant.argumento[1]	=	String(gi_CodExport)
istr_mant.argumento[2]	=	String(uo_SelPlantas.Codigo)

IF dw_1.RowCount() > 0 THEN
	istr_mant.agrega	= False
	istr_mant.borra	= False

	OpenWithParm(iw_mantencion, istr_mant)
END IF
end event

event ue_nuevo;call super::ue_nuevo;istr_mant.borra	= False
istr_mant.agrega	= True

istr_mant.argumento[1]	=	String(gi_CodExport)
istr_mant.argumento[2]	=	String(uo_SelPlantas.Codigo)

OpenWithParm(iw_mantencion, istr_mant)

IF dw_1.RowCount() > 0 and pb_eliminar.Enabled = FALSE THEN
	pb_eliminar.Enabled	= TRUE
	pb_grabar.Enabled		= TRUE
END IF

dw_1.SetRow(il_fila)
dw_1.SelectRow(il_fila,True)

end event

type dw_1 from w_mant_tabla`dw_1 within w_mant_mues_ctlcallotes
integer x = 82
integer y = 360
integer width = 4512
integer height = 1432
integer taborder = 40
string dataobject = "dw_mues_tabla_ctlcallotes"
boolean hscrollbar = true
end type

type st_encabe from w_mant_tabla`st_encabe within w_mant_mues_ctlcallotes
integer width = 4114
end type

type pb_lectura from w_mant_tabla`pb_lectura within w_mant_mues_ctlcallotes
integer x = 4727
integer y = 188
integer taborder = 20
end type

event pb_lectura::clicked;call super::clicked;uo_SelPlantas.Bloquear(True)



end event

type pb_nuevo from w_mant_tabla`pb_nuevo within w_mant_mues_ctlcallotes
integer x = 4722
integer y = 460
integer taborder = 30
end type

event pb_nuevo::clicked;call super::clicked;uo_SelPlantas.Bloquear(False)

pb_insertar.Enabled	= False
pb_eliminar.Enabled	= False
pb_grabar.Enabled	= False
pb_imprimir.Enabled	= False

il_fila					= 0




end event

type pb_insertar from w_mant_tabla`pb_insertar within w_mant_mues_ctlcallotes
integer x = 4722
integer y = 640
integer taborder = 50
end type

type pb_eliminar from w_mant_tabla`pb_eliminar within w_mant_mues_ctlcallotes
integer x = 4722
integer y = 820
integer taborder = 60
end type

type pb_grabar from w_mant_tabla`pb_grabar within w_mant_mues_ctlcallotes
integer x = 4722
integer y = 1000
integer taborder = 70
end type

type pb_imprimir from w_mant_tabla`pb_imprimir within w_mant_mues_ctlcallotes
integer x = 4722
integer y = 1180
integer taborder = 80
end type

type pb_salir from w_mant_tabla`pb_salir within w_mant_mues_ctlcallotes
integer x = 4722
integer y = 1564
integer taborder = 90
end type

type st_1 from statictext within w_mant_mues_ctlcallotes
integer x = 1605
integer y = 148
integer width = 233
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554431
long backcolor = 553648127
string text = "Planta"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type uo_selplantas from uo_seleccion_plantas within w_mant_mues_ctlcallotes
event destroy ( )
integer x = 1897
integer y = 132
integer height = 96
integer taborder = 20
boolean bringtotop = true
end type

on uo_selplantas.destroy
call uo_seleccion_plantas::destroy
end on

