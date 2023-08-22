$PBExportHeader$w_mant_despemba_packing_deta.srw
$PBExportComments$Mantención Detalle de Ordenes de Proceso
forward
global type w_mant_despemba_packing_deta from w_mant_detalle_csd
end type
type dw_2 from uo_dw within w_mant_despemba_packing_deta
end type
type pb_ins_det from picturebutton within w_mant_despemba_packing_deta
end type
type pb_eli_det from picturebutton within w_mant_despemba_packing_deta
end type
type st_1 from statictext within w_mant_despemba_packing_deta
end type
type em_total from editmask within w_mant_despemba_packing_deta
end type
type dw_3 from uo_dw within w_mant_despemba_packing_deta
end type
type st_4 from statictext within w_mant_despemba_packing_deta
end type
type cb_desaplica from commandbutton within w_mant_despemba_packing_deta
end type
type cb_desaplicatodo from commandbutton within w_mant_despemba_packing_deta
end type
type st_5 from statictext within w_mant_despemba_packing_deta
end type
type st_2 from statictext within w_mant_despemba_packing_deta
end type
type st_6 from statictext within w_mant_despemba_packing_deta
end type
type st_7 from statictext within w_mant_despemba_packing_deta
end type
type st_8 from statictext within w_mant_despemba_packing_deta
end type
type uo_selcliente from uo_seleccion_clientesprod within w_mant_despemba_packing_deta
end type
type uo_selplanta from uo_seleccion_plantas within w_mant_despemba_packing_deta
end type
type cb_1 from commandbutton within w_mant_despemba_packing_deta
end type
type sle_pallet from singlelineedit within w_mant_despemba_packing_deta
end type
type st_3 from statictext within w_mant_despemba_packing_deta
end type
type cb_aplicar from commandbutton within w_mant_despemba_packing_deta
end type
type cb_aplicatodo from commandbutton within w_mant_despemba_packing_deta
end type
end forward

global type w_mant_despemba_packing_deta from w_mant_detalle_csd
integer width = 3858
integer height = 1804
string title = "DETALLE ORDEN DE PROCESO"
boolean controlmenu = true
event ue_nuevo_detalle ( )
event ue_borra_detalle ( )
dw_2 dw_2
pb_ins_det pb_ins_det
pb_eli_det pb_eli_det
st_1 st_1
em_total em_total
dw_3 dw_3
st_4 st_4
cb_desaplica cb_desaplica
cb_desaplicatodo cb_desaplicatodo
st_5 st_5
st_2 st_2
st_6 st_6
st_7 st_7
st_8 st_8
uo_selcliente uo_selcliente
uo_selplanta uo_selplanta
cb_1 cb_1
sle_pallet sle_pallet
st_3 st_3
cb_aplicar cb_aplicar
cb_aplicatodo cb_aplicatodo
end type
global w_mant_despemba_packing_deta w_mant_despemba_packing_deta

type variables
DataWindowChild	idwc_especie, idwc_calibre, idwc_lineapacking, idwc_camara, idwc_envase, &
						idwc_variedad, idwc_calidad
uo_ProdCuarteles		iuo_Cuartel
uo_Productores		iuo_Productor

String				is_rutprod
Integer			il_Fila_det
Boolean  			lb_modifica	=	False
Long     			il_total

str_envase		istr_Envase

datastore		ids_CorrelMovim
end variables

forward prototypes
public subroutine captura_totalbultos ()
public function long busnuevofoliodespa (integer ai_cliente, integer ai_planta)
public subroutine verifica_cajas ()
end prototypes

event ue_borra_detalle();Integer	li_Fila

IF dw_2.RowCount() < 1 THEN RETURN

SetPointer(HourGlass!)

ib_borrar = True
w_main.SetMicroHelp("Validando la eliminación de detalle...")

Message.DoubleParm = 0

This.TriggerEvent ("ue_validaborrar_detalle")

IF Message.DoubleParm = -1 THEN RETURN

IF MessageBox("Eliminación de Registro", "Está seguro de Eliminar este Registro", Question!, YesNo!) = 1 THEN
	IF dw_2.DeleteRow(0) = 1 THEN
		ib_borrar = False
		
		w_main.SetMicroHelp("Borrando Registro...")
		SetPointer(Arrow!)
	ELSE
		ib_borrar = False
		MessageBox(This.Title,"No se puede borrar actual registro.")
	END IF

	IF dw_2.RowCount() = 0 THEN
		pb_eli_det.Enabled = False
	ELSE
		il_fila = dw_2.GetRow()
		il_fila_det = dw_2.GetRow()
	END IF
END IF
end event

public subroutine captura_totalbultos ();//Long	ll_Fila,ll_Total_Bultos, ll_bultos_ant
//
//ll_Fila	=	dw_2.RowCount()
//
//IF ll_Fila > 0 THEN
//	ll_Total_Bultos		=	dw_2.Object.total_bultos[ll_Fila]
//END IF
//
//ll_bultos_ant = dw_1.Object.orpr_canbul[il_Fila]
//IF isnull(ll_bultos_ant) THEN ll_bultos_ant=0
//	dw_1.Object.saldo[il_fila]       = ll_bultos_ant + dw_1.Object.saldo[il_fila] - ll_Total_Bultos
//	dw_1.Object.orpr_canbul[il_Fila]	= ll_Total_Bultos
//RETURN
end subroutine

public function long busnuevofoliodespa (integer ai_cliente, integer ai_planta);/* Busca Folio para hacer un  Despacho de pallet */

Integer	li_Planta
Long		ll_Numero, ll_Inicia, ll_Termin, ll_Actual, ll_Quedan, ll_despacho
Boolean	lb_Nulo

li_Planta	=	ai_planta
ll_Numero	=	0

select max(defe_numero)
into :ll_despacho
from dbo.despafrigoen
where plde_codigo = :li_planta;
		

ids_CorrelMovim.Retrieve(li_Planta,2)
IF ids_CorrelMovim.RowCount() > 0 THEN
	ll_Inicia	=	ids_CorrelMovim.Object.como_inicia[1]
	ll_Termin	=	ids_CorrelMovim.Object.como_termin[1]
	ll_Actual	=	ids_CorrelMovim.Object.como_actual[1]
	
	IF Isnull(ll_Inicia) THEN ll_Inicia	=	0
	IF Isnull(ll_Termin) THEN ll_Termin	=	0
	IF Isnull(ll_Actual) THEN ll_Actual	=	0
		
	IF Isnull(ll_despacho) OR String(ll_despacho) = '' OR ll_despacho < ll_Inicia THEN
		ll_Actual = ll_Inicia
	ELSE
		ll_Actual=	ll_despacho
	END IF	

	IF ll_Inicia >= 0 AND ll_Termin > 0 THEN
		IF ll_Actual	=	0	THEN
			ll_Actual	=	ll_Inicia + 1
		ELSE
			ll_Actual++		
		END IF
		
		IF ll_Actual >= ll_Inicia AND	ll_Actual <= ll_Termin	THEN
		
			IF ll_Actual > ll_Termin THEN
				Messagebox("Atención","No Existen Números de Folios Disponibles de Movimiento de Despacho",exclamation!) 			
				ll_Numero	=	0
				RETURN ll_Numero
			ELSEIF ll_Actual = ll_Termin THEN
				Messagebox("Atención","Ultimo Número de Folio Disponible de Movimiento de Despacho",exclamation!) 			
			END IF
			
			ll_Numero	=	ll_Actual
				
			ll_Quedan	=	(ll_Termin - ll_Actual)
			IF ll_Quedan <= 3 THEN
				Messagebox("Atención","Existen "+String(ll_Quedan)+" Número de Folio Disponible de Movimiento de Despacho",exclamation!) 			
			END IF
			
			ids_CorrelMovim.Object.como_actual[1]	=	ll_Actual
		ELSE
			Messagebox("Atención","Número de Folio Generado no se encuentra Disponible dentro del rango de Movimiento de Despacho",exclamation!) 
			ll_Numero	=	0
			RETURN ll_Numero
		END IF		
		
	ELSE
		Messagebox("Atención","Número de Folio Generado no se encuentra Disponible dentro del rango de Movimiento de Despacho",exclamation!) 
		ll_Numero	=	0
		RETURN ll_Numero	
	END IF
	
ELSE
	Messagebox("Atención","No Existe Ningún Número de Movimiento de Despacho",exclamation!) 
	ll_Numero	=	0
	RETURN ll_Numero	
END IF

RETURN ll_numero
end function

public subroutine verifica_cajas ();Integer 	li_i, li_TipoEnvase, li_CodiEnvase, li_CajasPallet
uo_tipopallenvase						luo_tipopallet
String		ls_TipoPallet
luo_tipopallet		=	Create uo_tipopallenvase

FOR li_i = 1 to dw_2.RowCount()
	
	ls_TipoPallet 	=	dw_2.Object.tpen_codigo[li_i]
	li_TipoEnvase	=	dw_2.Object.enva_tipoen[li_i]
	li_CodiEnvase	=	dw_2.Object.enva_codigo[li_i]
	
	IF NOT luo_tipopallet.Existe(li_TipoEnvase, li_CodiEnvase, ls_TipoPallet,FALSE,SQLCA) THEN
		MessageBox("Error", "No existe Tipo de Pallet para pallet seleccionado, imposible validar cajas.~n~rSe eliminara el pallet " + String(dw_2.Object.paen_numero[li_i]) +" del despacho.")
		dw_2.DeleteRow(li_i)
		li_i = li_i - 1
	ELSEIF luo_tipopallet.CajasPallet <> dw_2.Object.paen_ccajas[li_i] AND dw_2.Object.paen_tipopa[li_i] = 1 THEN
		MessageBox("Error","Cajas del detalle no corresponden a las cajas según Tipo de Pallet .~n~rSe eliminara el pallet " + String(dw_2.Object.paen_numero[li_i])+" del despacho.")
		dw_2.DeleteRow(li_i)
		li_i = li_i - 1
	END IF
	
NEXT
end subroutine

on w_mant_despemba_packing_deta.create
int iCurrent
call super::create
this.dw_2=create dw_2
this.pb_ins_det=create pb_ins_det
this.pb_eli_det=create pb_eli_det
this.st_1=create st_1
this.em_total=create em_total
this.dw_3=create dw_3
this.st_4=create st_4
this.cb_desaplica=create cb_desaplica
this.cb_desaplicatodo=create cb_desaplicatodo
this.st_5=create st_5
this.st_2=create st_2
this.st_6=create st_6
this.st_7=create st_7
this.st_8=create st_8
this.uo_selcliente=create uo_selcliente
this.uo_selplanta=create uo_selplanta
this.cb_1=create cb_1
this.sle_pallet=create sle_pallet
this.st_3=create st_3
this.cb_aplicar=create cb_aplicar
this.cb_aplicatodo=create cb_aplicatodo
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_2
this.Control[iCurrent+2]=this.pb_ins_det
this.Control[iCurrent+3]=this.pb_eli_det
this.Control[iCurrent+4]=this.st_1
this.Control[iCurrent+5]=this.em_total
this.Control[iCurrent+6]=this.dw_3
this.Control[iCurrent+7]=this.st_4
this.Control[iCurrent+8]=this.cb_desaplica
this.Control[iCurrent+9]=this.cb_desaplicatodo
this.Control[iCurrent+10]=this.st_5
this.Control[iCurrent+11]=this.st_2
this.Control[iCurrent+12]=this.st_6
this.Control[iCurrent+13]=this.st_7
this.Control[iCurrent+14]=this.st_8
this.Control[iCurrent+15]=this.uo_selcliente
this.Control[iCurrent+16]=this.uo_selplanta
this.Control[iCurrent+17]=this.cb_1
this.Control[iCurrent+18]=this.sle_pallet
this.Control[iCurrent+19]=this.st_3
this.Control[iCurrent+20]=this.cb_aplicar
this.Control[iCurrent+21]=this.cb_aplicatodo
end on

on w_mant_despemba_packing_deta.destroy
call super::destroy
destroy(this.dw_2)
destroy(this.pb_ins_det)
destroy(this.pb_eli_det)
destroy(this.st_1)
destroy(this.em_total)
destroy(this.dw_3)
destroy(this.st_4)
destroy(this.cb_desaplica)
destroy(this.cb_desaplicatodo)
destroy(this.st_5)
destroy(this.st_2)
destroy(this.st_6)
destroy(this.st_7)
destroy(this.st_8)
destroy(this.uo_selcliente)
destroy(this.uo_selplanta)
destroy(this.cb_1)
destroy(this.sle_pallet)
destroy(this.st_3)
destroy(this.cb_aplicar)
destroy(this.cb_aplicatodo)
end on

event ue_recuperadatos;call super::ue_recuperadatos;Integer	li_Fila, li_FilaClasif, li_CtaFila, li_seguimiento, li_null
string   ls_null
Long ll_prod_cod
setnull(ls_null)

li_Fila	=	dw_3.Retrieve(Integer(istr_Mant.Argumento[2]), &
							    Integer(istr_Mant.Argumento[1]), 1)
	
IF li_Fila > 0 THEN
	cb_aplicar.Enabled		=	True
	cb_aplicatodo.Enabled	=	True
END IF

li_CtaFila	=	dw_2.RowCount()

FOR li_Fila	=	1 TO li_CtaFila
	li_FilaClasif	=	dw_3.Find("paen_numero = "	+	String(dw_2.Object.paen_numero[li_Fila])	+	" AND "	+	&
									  	 "plde_codigo = "	+	String(dw_2.Object.plde_codigo[li_Fila])	+	" AND "	+	&
									    "clie_codigo = "	+	String(dw_2.Object.clie_codigo[li_Fila])	,					&
									  	 1, dw_3.RowCount())
	
	IF li_FilaClasif > 0 THEN
		dw_3.DeleteRow(li_FilaClasif)
	END IF
NEXT

IF dw_3.RowCount() > 0 THEN
	cb_aplicar.Enabled			=	True
	cb_aplicatodo.Enabled		=	True
	
END IF

IF dw_2.RowCount() > 0 THEN
	cb_desaplica.Enabled			=	True
	cb_desaplicatodo.Enabled	=	True
	
END IF

Captura_TotalBultos()
end event

event ue_deshace();call super::ue_deshace;IF UpperBound(ias_campo) > 0 THEN
END IF
end event

event open;/* 
Argumentos
istr_Mant.Argumento[1]	=	Código Planta
istr_Mant.Argumento[2]	=	Especie
istr_Mant.Argumento[3]	=	Fecha Programa
istr_Mant.Argumento[4]	=	Fecha Proceso
istr_Mant.Argumento[5]	=	Variedad
istr_Mant.Argumento[6]	=	Productor
istr_Mant.Argumento[7]	=	Periodo Frio
istr_Mant.Argumento[8]	=	Tratamiento Frio
istr_Mant.Argumento[9]	=	Numero de Orden
istr_Mant.Argumento[13]	=	Cliente
*/

Boolean	lb_Cerrar
Integer	li_Cliente

x	= 100
y	= 450

IF IsNull(uo_Selplanta.Codigo) THEN lb_Cerrar	=	True
IF IsNull(uo_SelCliente.Codigo) THEN lb_Cerrar	=	True

IF lb_Cerrar THEN
	Close(This)
ELSE
	istr_mant = Message.PowerObjectParm
	
	uo_SelPlanta.Seleccion(False,False)
	uo_SelCliente.Seleccion(False,False)
	
	uo_SelPlanta.dw_Seleccion.Object.codigo[1]	=	Long(istr_Mant.Argumento[1])
	uo_SelCliente.dw_Seleccion.Object.Codigo[1]		=	Integer(istr_Mant.Argumento[2])
	
	uo_SelPlanta.Codigo										=	Long(istr_Mant.Argumento[1])
	uo_SelCliente.Codigo										=	Integer(istr_Mant.Argumento[2])
	
	This.Icon	=	Gstr_apl.Icono
	
	PostEvent("ue_recuperadatos")
	
	li_Cliente	=	Integer(istr_mant.argumento[2])
	
	dw_1.SetTransObject(Sqlca)
	
	dw_2.Modify("datawindow.message.title='Error '+ is_titulo")
	dw_2.SetRowFocusIndicator(Hand!)
	
	dw_3.SetRowFocusIndicator(Hand!)
	
	dw_2.SetTransObject(Sqlca)
	istr_mant.dw.ShareData(dw_2)
	
	dw_3.SetTransObject(Sqlca)
	
	ids_CorrelMovim						=	CREATE	DataStore
	ids_CorrelMovim.DataObject			=	'dw_mues_correlmoviemientos_despa'
	ids_CorrelMovim.SetTransObject(sqlca)
END IF

end event

event closequery;IF Not istr_mant.Borra THEN

	IF istr_mant.Agrega AND istr_mant.Respuesta <> 1 THEN 
		dw_1.DeleteRow(il_fila)
		dw_2.RowsMove(1,dw_2.RowCount(),Primary!,dw_2,1,Delete!)
		
		IF dw_1.RowCount() > 0 THEN
			dw_1.SelectRow(0, False)
			dw_1.SelectRow(dw_1.RowCount(), True)
		END IF
		
		RETURN
	END IF

	IF ib_Modifica AND istr_mant.Respuesta = 1 THEN
		This.TriggerEvent("ue_guardar")
		
		IF Message.DoubleParm = -1 THEN Message.ReturnValue = 1
		
		RETURN
	ELSEIF istr_mant.Respuesta = 2 THEN
		This.TriggerEvent("ue_deshace")
	END IF
END IF
end event

event ue_nuevo;call super::ue_nuevo;//dw_1.Object.plde_codigo[il_Fila]	=	Integer(istr_Mant.Argumento[1])

end event

event ue_borrar();call super::ue_borrar;Long	ll_fila1

if dw_2.rowcount() < 1 then return
if messagebox("Borrar registro(s)","Desea Eliminar la fila seleccionada ?",&
				exclamation!,yesno!,2) <> 1 then
	return
end if
setpointer(hourglass!)
ib_borrar = true
w_main.setmicrohelp("Validando la eliminación...")
message.doubleparm = 0
this.triggerevent ("ue_validaborrar")
if message.doubleparm = -1 then return

ll_fila1=dw_2.GetRow()

IF dw_2.RowsCopy(ll_fila1,ll_fila1,Primary!,dw_3,dw_3.RowCount()+1,Primary!) > 0 THEN
	dw_2.DeleteRow(ll_fila1)
	w_main.setmicrohelp("Registro Borrado.")
	lb_modifica = TRUE
	setpointer(Arrow!)
else
	ib_borrar = false
	messagebox(this.title,"No se puede borrar registro actual.")
end if

end event

event ue_antesguardar;call super::ue_antesguardar;Integer	li_Secuencia, li_Fila

li_Secuencia	=	1

FOR li_Fila = 1 TO dw_2.RowCount()
	IF lb_modifica=FALSE THEN dw_2.Setitemstatus(li_fila, 0, Primary!, Notmodified!)
	li_Secuencia ++
	
NEXT

IF lb_modifica=FALSE THEN
	dw_1.Setitemstatus(il_fila, 0, Primary!, Notmodified!)
	
END IF

dw_2.SetFilter("")
dw_2.Filter()
end event

event ue_guardar();SetPointer(HourGlass!)

Message.DoubleParm = 0

w_main.SetMicroHelp("Grabando información...")
TriggerEvent("ue_antesguardar")

IF Message.DoubleParm = -1 THEN RETURN
end event

event resize;Integer		li_posic_x, li_posic_y, li_Ancho = 300, li_Alto = 245, li_Siguiente = 255

This.Height			=	dw_1.Height + 300
This.Width			=	dw_1.width + 800

li_posic_x			=	This.WorkSpaceWidth() - 400
li_posic_y			=	108

pb_acepta.width	=	li_Ancho
pb_acepta.height	=	li_Alto
pb_acepta.x			=	li_posic_x
pb_acepta.y			=	li_posic_y

pb_cancela.x		=	pb_acepta.x
pb_cancela.y		=	pb_acepta.y + li_Siguiente
pb_cancela.width	=	li_Ancho
pb_cancela.height	=	li_Alto

pb_salir.x			=	pb_acepta.x
pb_salir.y			=	pb_cancela.y + li_Siguiente
pb_salir.width		=	li_Ancho
pb_salir.height		=	li_Alto


end event

type pb_ultimo from w_mant_detalle_csd`pb_ultimo within w_mant_despemba_packing_deta
boolean visible = false
integer x = 3730
integer y = 100
end type

type pb_siguiente from w_mant_detalle_csd`pb_siguiente within w_mant_despemba_packing_deta
boolean visible = false
integer x = 3730
integer y = 100
end type

type pb_anterior from w_mant_detalle_csd`pb_anterior within w_mant_despemba_packing_deta
boolean visible = false
integer x = 3730
integer y = 100
end type

type pb_primero from w_mant_detalle_csd`pb_primero within w_mant_despemba_packing_deta
boolean visible = false
integer x = 3730
integer y = 100
end type

type pb_cancela from w_mant_detalle_csd`pb_cancela within w_mant_despemba_packing_deta
integer x = 3355
integer y = 352
integer taborder = 50
boolean enabled = false
end type

event pb_cancela::clicked;istr_mant.respuesta = 2

dw_2.SetFilter("")
dw_2.Filter()

CloseWithReturn(Parent, istr_mant)
end event

type pb_acepta from w_mant_detalle_csd`pb_acepta within w_mant_despemba_packing_deta
integer x = 3351
integer y = 108
integer taborder = 40
boolean default = false
alignment htextalign = center!
end type

event pb_acepta::clicked;istr_mant.respuesta = 1

IF istr_mant.agrega THEN
	Parent.TriggerEvent("ue_nuevo")
ELSE
	Parent.TriggerEvent("ue_antesguardar")
	CloseWithReturn(Parent, istr_mant)
END IF
end event

type pb_salir from w_mant_detalle_csd`pb_salir within w_mant_despemba_packing_deta
integer x = 3365
integer y = 608
integer taborder = 60
end type

event pb_salir::clicked;IF istr_mant.Agrega THEN 
	//Descuenta último Lote generado
	istr_mant.argumento[7]	=	String(Integer(istr_mant.argumento[7])-1)
END IF

CALL SUPER::Clicked
end event

type dw_1 from w_mant_detalle_csd`dw_1 within w_mant_despemba_packing_deta
boolean visible = false
integer x = 91
integer y = 232
integer width = 2990
integer height = 1616
string dataobject = "dw_mues_ordenproceso"
end type

type dw_2 from uo_dw within w_mant_despemba_packing_deta
integer x = 448
integer y = 932
integer width = 2811
integer height = 704
integer taborder = 20
boolean bringtotop = true
boolean titlebar = true
string title = "PALLETS SELECCIONADOS"
string dataobject = "dw_mues_palletencab_packing"
boolean hscrollbar = true
boolean livescroll = true
end type

event clicked;call super::clicked;IF row > 0 THEN
	IF IsSelected(row) THEN
		SelectRow(row,False)
	ELSE
		SelectRow(row,True)
	END IF
END IF
end event

event itemerror;call super::itemerror;RETURN 1
end event

event losefocus;call super::losefocus;Accepttext()
end event

event rowfocuschanged;call super::rowfocuschanged;ib_datos_ok = True

IF This.RowCount() < 1 OR CurrentRow = 0 OR ib_borrar THEN
	ib_datos_ok = False
ELSE
	il_fila_det = CurrentRow
END IF
end event

type pb_ins_det from picturebutton within w_mant_despemba_packing_deta
boolean visible = false
integer x = 3355
integer y = 1008
integer width = 302
integer height = 244
integer taborder = 30
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string picturename = "\Desarrollo 17\Imagenes\Botones\Signo Mas.png"
string disabledname = "\Desarrollo 17\Imagenes\Botones\Signo Mas-bn.png"
alignment htextalign = left!
end type

event clicked;Parent.TriggerEvent("ue_nuevo_detalle")
end event

type pb_eli_det from picturebutton within w_mant_despemba_packing_deta
boolean visible = false
integer x = 3355
integer y = 1316
integer width = 302
integer height = 244
integer taborder = 70
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string picturename = "\Desarrollo 17\Imagenes\Botones\Signo Menos.png"
string disabledname = "\Desarrollo 17\Imagenes\Botones\Signo Menos-bn.png"
alignment htextalign = left!
end type

event clicked;Parent.TriggerEvent("ue_borra_detalle")
end event

type st_1 from statictext within w_mant_despemba_packing_deta
boolean visible = false
integer x = 599
integer y = 708
integer width = 402
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
string text = "Total Cajas"
boolean focusrectangle = false
end type

type em_total from editmask within w_mant_despemba_packing_deta
boolean visible = false
integer x = 978
integer y = 696
integer width = 402
integer height = 84
integer taborder = 50
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
alignment alignment = right!
borderstyle borderstyle = stylelowered!
string mask = "#######"
end type

event modified;dw_2.Enabled 			= TRUE
pb_ins_det.Enabled	= TRUE
pb_eli_det.Enabled	= TRUE

il_total= Long(em_total.text)
end event

type dw_3 from uo_dw within w_mant_despemba_packing_deta
integer x = 448
integer y = 216
integer width = 2811
integer height = 716
integer taborder = 30
boolean bringtotop = true
boolean titlebar = true
string title = "PALLETS CLASIFICADOS"
string dataobject = "dw_mues_palletencab_existe_packing"
boolean hscrollbar = true
boolean livescroll = true
end type

event clicked;call super::clicked;IF row > 0 THEN
	IF IsSelected(row) THEN
		SelectRow(row,False)
	ELSE
		SelectRow(row,True)
	END IF
END IF
end event

event itemerror;call super::itemerror;RETURN 1
end event

event losefocus;call super::losefocus;Accepttext()
end event

event rowfocuschanged;call super::rowfocuschanged;ib_datos_ok = True

IF This.RowCount() < 1 OR CurrentRow = 0 OR ib_borrar THEN
	ib_datos_ok = False
ELSE
	il_fila_det = CurrentRow
END IF
end event

type st_4 from statictext within w_mant_despemba_packing_deta
integer x = 32
integer y = 928
integer width = 416
integer height = 704
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 16711680
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type cb_desaplica from commandbutton within w_mant_despemba_packing_deta
integer x = 64
integer y = 1156
integer width = 352
integer height = 104
integer taborder = 140
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string text = "Desaplica "
end type

event clicked;Parent.TriggerEvent("ue_borrar")

Captura_TotalBultos()
end event

type cb_desaplicatodo from commandbutton within w_mant_despemba_packing_deta
integer x = 64
integer y = 1316
integer width = 352
integer height = 104
integer taborder = 150
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string text = "Desapl. Todo"
end type

event clicked;Long		ll_filas

if dw_2.rowcount() < 1 then return
if messagebox("Borrar registro(s)","Desea Eliminar TODAS LAS FILAS asignadas ?",&
				exclamation!,yesno!,2) <> 1 then
	return
end if

SetPointer(hourglass!)

ib_borrar = true
w_main.setmicrohelp("Validando la eliminación...")
message.doubleparm = 0
Parent.triggerevent ("ue_validaborrar")
if message.doubleparm = -1 then return
dw_2.RowsCopy(1,dw_2.RowCount(),Primary!,dw_3,dw_3.RowCount()+1,Primary!)
dw_2.RowsMove(1,dw_2.RowCount(),Primary!,dw_2,1,Delete!)

cb_aplicar.Enabled	= True
cb_aplicatodo.Enabled	= True
lb_modifica = TRUE
dw_3.SetFocus()
end event

type st_5 from statictext within w_mant_despemba_packing_deta
boolean visible = false
integer x = 471
integer y = 332
integer width = 2784
integer height = 352
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

type st_2 from statictext within w_mant_despemba_packing_deta
integer x = 32
integer y = 24
integer width = 3227
integer height = 200
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 16711680
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_6 from statictext within w_mant_despemba_packing_deta
integer x = 73
integer y = 92
integer width = 224
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
boolean enabled = false
string text = "Cliente"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_7 from statictext within w_mant_despemba_packing_deta
integer x = 1271
integer y = 92
integer width = 224
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
boolean enabled = false
string text = "Planta"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_8 from statictext within w_mant_despemba_packing_deta
integer x = 2464
integer y = 92
integer width = 178
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
boolean enabled = false
string text = "Pallet"
boolean focusrectangle = false
end type

type uo_selcliente from uo_seleccion_clientesprod within w_mant_despemba_packing_deta
integer x = 293
integer y = 84
integer height = 84
integer taborder = 40
boolean bringtotop = true
boolean enabled = false
end type

on uo_selcliente.destroy
call uo_seleccion_clientesprod::destroy
end on

type uo_selplanta from uo_seleccion_plantas within w_mant_despemba_packing_deta
integer x = 1481
integer y = 84
integer height = 84
integer taborder = 50
boolean bringtotop = true
boolean enabled = false
end type

on uo_selplanta.destroy
call uo_seleccion_plantas::destroy
end on

type cb_1 from commandbutton within w_mant_despemba_packing_deta
integer x = 3003
integer y = 72
integer width = 224
integer height = 112
integer taborder = 60
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Aplica"
end type

event clicked;IF dw_3.RowCount() = 1 THEN
	dw_3.SelectRow(0, False)
	dw_3.SelectRow(1, True)
	cb_aplicar.TriggerEvent(Clicked!)
	
ELSEIF dw_3.RowCount() > 1 THEN
	MessageBox("Error", "Existe mas de un pallet que corresponde con el Nro. de Folio ingresado.~r~n" + &
							  "Por Favor, Ingrese o Seleccione otro.")
							  
ELSE
	MessageBox("Error", "No existe el Nro. de Folio ingresado o este ya ha sido despachado.~r~n" + &
							  "Por Favor, Ingrese o Seleccione otro.")
							  
END IF

dw_3.SetFilter("")
dw_3.Filter()
sle_pallet.Text	=	""
sle_pallet.SetFocus()
end event

type sle_pallet from singlelineedit within w_mant_despemba_packing_deta
event ue_cambio pbm_keyup
integer x = 2661
integer y = 84
integer width = 325
integer height = 84
integer taborder = 50
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
borderstyle borderstyle = stylelowered!
end type

event ue_cambio;IF This.Text = "" THEN
	dw_3.SetFilter("")
ELSE
	dw_3.SetFilter("String(paen_numero, '00000000') like '%" + This.Text + "%'")
END IF

dw_3.Filter()
end event

type st_3 from statictext within w_mant_despemba_packing_deta
integer x = 32
integer y = 224
integer width = 416
integer height = 704
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 16711680
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type cb_aplicar from commandbutton within w_mant_despemba_packing_deta
integer x = 64
integer y = 468
integer width = 352
integer height = 104
integer taborder = 120
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string text = "Aplica"
end type

event clicked;Long		ll_fila1, ll_fila2
Integer	li_causal

ll_fila2 = dw_3.GetSelectedRow(0)

IF ll_fila2 = 0 THEN
	MessageBox("Error de Consistencia","Debe seleccionar Lotes Clasificados previamente ")
	RETURN
END IF

SetPointer(HourGlass!)

DO WHILE ll_fila2 > 0
	ll_fila1	=	dw_2.RowCount() + 1

	dw_3.RowsMove(ll_fila2,ll_fila2,Primary!,dw_2,ll_fila1,Primary!)

	cb_desaplica.Enabled	= True
	cb_desaplicatodo.Enabled	= True

	ll_fila2 = dw_3.GetSelectedRow(0)
LOOP

lb_modifica = TRUE
Captura_TotalBultos()

dw_2.SetFocus()


end event

type cb_aplicatodo from commandbutton within w_mant_despemba_packing_deta
integer x = 64
integer y = 628
integer width = 352
integer height = 104
integer taborder = 130
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string text = "Aplica Todos"
end type

event clicked;Long		ll_fila2,ll_fila1
Boolean	lb_respuesta = True

ll_fila2 = dw_3.Rowcount()

IF ll_fila2 = 0 THEN
	MessageBox("Error de Consistencia","No Existe Lotes Clasificados")
	RETURN 
END IF

SetPointer(HourGlass!)

IF lb_respuesta THEN
	ll_fila1	=	dw_2.RowCount() + 1
	dw_3.RowsMove(1,ll_fila2,Primary!,dw_2,ll_fila1,Primary!)
	
	cb_desaplica.Enabled	= True
	cb_desaplicatodo.Enabled	= True

	dw_2.SetFocus()
ELSE
	dw_3.SelectRow(ll_fila2,False)
	dw_3.SetFocus()
END IF

lb_modifica = TRUE
Captura_TotalBultos()

dw_2.SetFocus()


end event

