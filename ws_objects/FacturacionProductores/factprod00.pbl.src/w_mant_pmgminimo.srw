$PBExportHeader$w_mant_pmgminimo.srw
forward
global type w_mant_pmgminimo from window
end type
type uo_selplanta from uo_seleccion_plantas within w_mant_pmgminimo
end type
type st_3 from statictext within w_mant_pmgminimo
end type
type pb_3 from picturebutton within w_mant_pmgminimo
end type
type pb_2 from picturebutton within w_mant_pmgminimo
end type
type dw_1 from uo_dw within w_mant_pmgminimo
end type
type st_2 from statictext within w_mant_pmgminimo
end type
end forward

global type w_mant_pmgminimo from window
integer x = 46
integer y = 48
integer width = 3136
integer height = 1404
boolean titlebar = true
string title = "Mantención Parametros de Fruta Granel"
boolean controlmenu = true
boolean minbox = true
long backcolor = 16777215
event ue_antesguardar pbm_custom75
event ue_guardar pbm_custom11
event ue_recuperadatos pbm_custom15
event ue_imprimir pbm_custom03
event ue_validapassword ( )
uo_selplanta uo_selplanta
st_3 st_3
pb_3 pb_3
pb_2 pb_2
dw_1 dw_1
st_2 st_2
end type
global w_mant_pmgminimo w_mant_pmgminimo

type variables
String				is_rutrl1, is_rutrl2

Integer 				ii_cliente

DataWindowChild   idwc_planta, idwc_especie, idwc_variedad, idwc_serplanta, &
 						idwc_produc, idwc_exporta, idwc_planta2, idwc_transportista, &
						idwc_condicion, idwc_tipoenvase, idwc_packing,idwc_cliente,&
						idwc_coneccion
end variables

forward prototypes
protected function boolean wf_actualiza_db ()
public subroutine wf_cargaespecies ()
public function boolean wf_validaespecie (integer ai_especie)
end prototypes

event ue_guardar;String ls_plantanomb
Integer li_plantaadmi, li_Planta

IF dw_1.AcceptText() = -1 THEN RETURN -1

SetPointer(HourGlass!)

w_main.SetMicroHelp("Grabando información...")

IF wf_actualiza_db() THEN
	w_main.SetMicroHelp("Información Grabada.")
ELSE
	w_main.SetMicroHelp("No se puede Grabar información.")
	Message.DoubleParm = -1
	RETURN -1
END IF

RETURN 0
end event

event ue_recuperadatos;Long		ll_fila, respuesta

DO
	ll_fila	= dw_1.Retrieve(uo_SelPlanta.Codigo)
	
	IF ll_fila = -1 THEN
		respuesta = MessageBox("Error en Base de Datos", "No es posible conectar la Base de Datos.", Information!, RetryCancel!)
	ELSEIF ll_fila > 0 THEN
		dw_1.SetRow(1)
		dw_1.SetFocus()
		wf_CargaEspecies()
	ELSE
		dw_1.Reset()
		wf_CargaEspecies()
		dw_1.InsertRow(0)
	END IF
LOOP WHILE respuesta = 1

IF respuesta = 2 THEN Close(This)
end event

event ue_validapassword();str_mant	lstr_mant

lstr_mant.Argumento[1]	=	"Granel"
lstr_mant.Argumento[2]	=	gstr_paramplanta.Password

OpenWithParm(w_password, lstr_mant)

lstr_mant	=	Message.PowerObjectParm

IF lstr_mant.Respuesta = 0 THEN 
	Close(This)
ELSE
	TriggerEvent("ue_recuperadatos")
END IF
end event

protected function boolean wf_actualiza_db ();Boolean	lb_Autocommit, lb_retorno	=	True
Long		ll_filas

lb_Autocommit		=	sqlca.Autocommit
sqlca.Autocommit	=	False

If dw_1.update() <> -1 Then 
	
	wf_CargaEspecies()
	commit;
	
	If sqlca.sqlcode <> 0 Then
		F_ErrorBaseDatos(sqlca,this.title)
		lb_retorno	=	false
	Else
		lb_retorno	=	true
	End If 
Else
	RollBack;
	
	If sqlca.sqlcode <> 0 Then F_ErrorBaseDatos(sqlca,this.title)
	lb_retorno	=	false
End If

sqlca.Autocommit	=	lb_Autocommit

If lb_retorno Then ParamPlanta()

Return lb_retorno


end function

public subroutine wf_cargaespecies ();Long		ll_fila, ll_find
Integer	li_especie, li_planta
String		ls_nombre


dw_1.Reset()
dw_1.Retrieve(uo_SelPlanta.Codigo)

FOR ll_fila = 1 TO idwc_especie.RowCount()
	li_especie	=	idwc_especie.GetItemNumber(ll_fila, "espe_codigo")
	ls_nombre	=	idwc_especie.GetItemString(ll_fila, "espe_nombre")
	
	ll_Find		=	dw_1.Find("espe_codigo = " + String(li_especie), 1, dw_1.RowCount())
	If ll_Find = 0 Then
		ll_find											=	dw_1.InsertRow(0)
		dw_1.Object.espe_codigo[ll_find]		=	li_especie
		dw_1.Object.espe_nombre[ll_find]	=	ls_nombre
		dw_1.Object.plde_codigo[ll_find]		=	li_Planta
	End If
NEXT

dw_1.SetSort("espe_codigo asc")
dw_1.Sort()
end subroutine

public function boolean wf_validaespecie (integer ai_especie);Integer	li_existe

SELECT DISTINCT espe_codigo
  INTO :li_existe
  FROM dbo.spro_movtofrutagranenca
 WHERE espe_codigo = :ai_especie
 USING sqlca;

IF sqlca.sqlcode = -1 THEN
	F_ErrorBaseDatos(sqlca,this.title)
	Return False
END IF

IF li_existe = ai_especie THEN
	MessageBox("Control de Datos", "No se puede eliminar control de bins para especie " + String(ai_especie) + &
											 " ya que posee movimientos de fruta asociados.", StopSign!)
	Return False
ELSE
	Return True
END IF
end function

on w_mant_pmgminimo.create
this.uo_selplanta=create uo_selplanta
this.st_3=create st_3
this.pb_3=create pb_3
this.pb_2=create pb_2
this.dw_1=create dw_1
this.st_2=create st_2
this.Control[]={this.uo_selplanta,&
this.st_3,&
this.pb_3,&
this.pb_2,&
this.dw_1,&
this.st_2}
end on

on w_mant_pmgminimo.destroy
destroy(this.uo_selplanta)
destroy(this.st_3)
destroy(this.pb_3)
destroy(this.pb_2)
destroy(this.dw_1)
destroy(this.st_2)
end on

event open;Boolean 	lb_Cerrar

If IsNull(uo_SelPlanta.Codigo) Then lb_Cerrar = True

If lb_Cerrar Then
	Close(This)
Else
	x				= 0
	y				= 0
	
	dw_1.SetTransObject(SQLCA)
	dw_1.SetRowFocusIndicator(Hand!)
	
	uo_SelPlanta.Seleccion(False, False)
	uo_SelPlanta.Inicia(gi_CodPlanta)
	
	If Not IsNull(gstr_paramplanta.Password) And Trim(gstr_paramplanta.Password) <> '' Then PostEvent("ue_validapassword")
	
End If
end event

event closequery;ParamPlanta()
end event

type uo_selplanta from uo_seleccion_plantas within w_mant_pmgminimo
integer x = 457
integer y = 68
integer height = 96
integer taborder = 60
end type

on uo_selplanta.destroy
call uo_seleccion_plantas::destroy
end on

event ue_cambio;call super::ue_cambio;Parent.TriggerEvent("ue_recuperadatos")
end event

type st_3 from statictext within w_mant_pmgminimo
integer x = 197
integer y = 84
integer width = 352
integer height = 72
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Planta"
boolean focusrectangle = false
end type

type pb_3 from picturebutton within w_mant_pmgminimo
event ue_mousemove pbm_mousemove
integer x = 2734
integer y = 132
integer width = 302
integer height = 244
integer taborder = 40
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string picturename = "\Desarrollo 17\Imagenes\Botones\Aceptar base datos.png"
string disabledname = "\Desarrollo 17\Imagenes\Botones\Aceptar base datos-bn.png"
alignment htextalign = left!
end type

event ue_mousemove;w_main.SetMicroHelp("Grabar actual información")
end event

event clicked;Parent.TriggerEvent("ue_guardar")

end event

type pb_2 from picturebutton within w_mant_pmgminimo
event ue_mousemove pbm_mousemove
integer x = 2734
integer y = 532
integer width = 302
integer height = 244
integer taborder = 50
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string picturename = "\Desarrollo 17\Imagenes\Botones\Apagar.png"
string disabledname = "\Desarrollo 17\Imagenes\Botones\Apagar-bn.png"
alignment htextalign = left!
end type

event ue_mousemove;w_main.SetMicroHelp("Salir de Parametros")
end event

event clicked;Close(Parent)
end event

type dw_1 from uo_dw within w_mant_pmgminimo
integer x = 55
integer y = 312
integer width = 2574
integer height = 924
integer taborder = 20
boolean bringtotop = true
boolean titlebar = true
string title = "Detalle de Manejo de Fruta Por Especie"
string dataobject = "dw_mues_spro_paramplantadeta"
boolean livescroll = true
end type

type st_2 from statictext within w_mant_pmgminimo
integer x = 55
integer y = 28
integer width = 2574
integer height = 184
integer textsize = -14
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 16711680
alignment alignment = center!
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

