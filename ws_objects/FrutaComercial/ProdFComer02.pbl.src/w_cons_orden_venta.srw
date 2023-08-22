$PBExportHeader$w_cons_orden_venta.srw
forward
global type w_cons_orden_venta from w_para_informes
end type
type dw_1 from datawindow within w_cons_orden_venta
end type
type dw_2 from datawindow within w_cons_orden_venta
end type
type cb_cierre from commandbutton within w_cons_orden_venta
end type
type cb_abrir from commandbutton within w_cons_orden_venta
end type
end forward

global type w_cons_orden_venta from w_para_informes
integer width = 3122
integer height = 1604
dw_1 dw_1
dw_2 dw_2
cb_cierre cb_cierre
cb_abrir cb_abrir
end type
global w_cons_orden_venta w_cons_orden_venta

type variables


datawindowchild idwc_planta


String is_cierre = "1"
end variables

forward prototypes
public function boolean existemovimiento (long al_numero)
end prototypes

public function boolean existemovimiento (long al_numero);Integer li_planta
Long     ll_nrodocto

li_planta  = dw_1.Object.plde_codigo[1]

SELECT	odfc_numero
	INTO	:ll_nrodocto
	FROM	dba.spro_ordenventacomenca
	WHERE	plde_codigo	=	:li_planta
	AND	odfc_numero	=	:al_numero;

IF SqlCa.SQLCode = -1 THEN
	F_ErrorBaseDatos(SqlCa, "Lectura de Tabla spro_ordenventacomenca")

	RETURN False
ELSEIF SqlCa.SQLCode = 100 THEN

	RETURN False
END IF

RETURN True
end function

on w_cons_orden_venta.create
int iCurrent
call super::create
this.dw_1=create dw_1
this.dw_2=create dw_2
this.cb_cierre=create cb_cierre
this.cb_abrir=create cb_abrir
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_1
this.Control[iCurrent+2]=this.dw_2
this.Control[iCurrent+3]=this.cb_cierre
this.Control[iCurrent+4]=this.cb_abrir
end on

on w_cons_orden_venta.destroy
call super::destroy
destroy(this.dw_1)
destroy(this.dw_2)
destroy(this.cb_cierre)
destroy(this.cb_abrir)
end on

event open;call super::open;
is_cierre =Message.StringParm			

dw_1.GetChild("plde_codigo",idwc_planta)
idwc_planta.SettransObject(SQLCA)
IF idwc_planta.Retrieve() = 0 THEN
	idwc_planta.InsertRow(0)
END If	


dw_1.SetTransObject(SQLCA)
dw_1.InsertRow(0)
dw_2.SetTransObject(SQLCA)


dw_1.Object.plde_codigo[1] = gstr_paramplanta.codigoplanta

end event

type st_titulo from w_para_informes`st_titulo within w_cons_orden_venta
integer x = 101
integer y = 80
integer width = 2546
string text = "Consulta  de Ordenes de Venta"
end type

type pb_acepta from w_para_informes`pb_acepta within w_cons_orden_venta
integer x = 2784
integer taborder = 20
string picturename = "\desarrollo\bmp\disksave.bmp"
string disabledname = "\desarrollo\bmp\disksavd.bmp"
end type

event pb_acepta::clicked;Long		ll_fila_d, ll_fila_e, respuesta, ll_numero
Integer  li_planta
String	ls_Null
SetNull(ls_Null)

IF isnull(dw_1.Object.odfc_numero[1]) OR dw_1.Object.odfc_numero[1] = 0 THEN
	MessageBoX("Atención","Ingrese un número de orden de venta.")
	RETURN 1
END IF	

DO
	li_planta = dw_1.Object.plde_codigo[1]
	ll_numero = dw_1.Object.odfc_numero[1]
	
	dw_1.SetRedraw(False)
	
	ll_fila_e	=	dw_1.Retrieve(li_planta, ll_numero)
	
	IF ll_fila_e = -1 THEN
		respuesta = MessageBox(	"Error en Base de Datos", "No es posible conectar la Base de Datos.", &
										Information!, RetryCancel!)
	ELSE
		
		DO
			ll_fila_d	=	dw_2.Retrieve(li_planta, ll_numero)
			
			IF ll_fila_d = -1 THEN
				respuesta = MessageBox(	"Error en Base de Datos", "No es posible conectar la Base de Datos.", &
												Information!, RetryCancel!)
			ELSE
				
				IF dw_1.RowCount() > 0 THEN
					IF dw_1.Object.odfc_pesfac[1] = 1 THEN
						dw_2.Object.ofcd_bultos.visible = TRUE
						dw_2.Object.ofcd_tkilos.visible = FALSE
						dw_2.Object.t_kilos.visible 	  = FALSE
						dw_2.Object.t_bultos.visible 	  = TRUE
					ELSE
						dw_2.Object.ofcd_bultos.visible = FALSE
						dw_2.Object.ofcd_tkilos.visible = TRUE
						dw_2.Object.t_kilos.visible 	  = TRUE
						dw_2.Object.t_bultos.visible 	  = FALSE
					END IF	
					
					IF is_cierre = "1" THEN
						IF dw_1.Object.odfc_estado[1] = 1 THEN cb_cierre.visible = TRUE
					ELSE
						IF dw_1.Object.odfc_estado[1] = 0 THEN cb_abrir.visible = TRUE
					END IF
					
				END IF	
			END IF
			dw_2.SetRedraw(True)
		LOOP WHILE respuesta = 1
		
		IF respuesta = 2 THEN Close(parent)
	END IF
	dw_1.SetRedraw(True)
LOOP WHILE respuesta = 1

IF respuesta = 2 THEN Close(parent)


end event

type pb_salir from w_para_informes`pb_salir within w_cons_orden_venta
integer x = 2784
integer taborder = 40
end type

type dw_1 from datawindow within w_cons_orden_venta
integer x = 105
integer y = 220
integer width = 2537
integer height = 532
integer taborder = 10
boolean bringtotop = true
string title = "none"
string dataobject = "dw_cons_ordenventa"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event itemchanged;String	ls_Columna, ls_Null, ls_Fecha

SetNull(ls_Null)

ls_Columna = dwo.name

CHOOSE CASE ls_Columna
		
	CASE "odfc_numero"
		
		IF NOT ExisteMovimiento(Long(Data)) THEN
			dw_2.Reset()
			cb_cierre.visible = FALSE
			cb_abrir.visible  = FALSE
			This.SetItem(1, ls_Columna, Long(ls_Null))
			RETURN 1
		END IF
		
END CHOOSE

end event

event itemerror;RETURN 1
end event

type dw_2 from datawindow within w_cons_orden_venta
integer x = 105
integer y = 776
integer width = 2542
integer height = 544
integer taborder = 30
boolean bringtotop = true
string title = "none"
string dataobject = "dw_info_ventas_real_orden"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type cb_cierre from commandbutton within w_cons_orden_venta
boolean visible = false
integer x = 2715
integer y = 1144
integer width = 265
integer height = 112
integer taborder = 50
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Cerrar"
end type

event clicked;

dw_1.Object.odfc_estado[1] = 0

IF dw_1.Update() = 1 THEN
	Commit;
ELSE
	RollBack;
   MessageBox("Atención","No se pudo cerrar la orden de venta.")	
END IF
end event

type cb_abrir from commandbutton within w_cons_orden_venta
boolean visible = false
integer x = 2715
integer y = 1144
integer width = 265
integer height = 112
integer taborder = 60
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Abrir"
end type

event clicked;
dw_1.Object.odfc_estado[1] = 1

IF dw_1.Update() = 1 THEN
	Commit;
ELSE
	RollBack;
   MessageBox("Atención","No se pudo Re-Abrir la orden de venta.")	
END IF
end event

