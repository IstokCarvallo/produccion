$PBExportHeader$w_maed_planificallegadaenca.srw
forward
global type w_maed_planificallegadaenca from w_mant_encab_deta_csd
end type
end forward

global type w_maed_planificallegadaenca from w_mant_encab_deta_csd
integer width = 3529
integer height = 2392
string title = "PLANIFICACION LLEGADA DESDE PACKING"
string menuname = ""
boolean maxbox = false
event ue_imprimir ( )
end type
global w_maed_planificallegadaenca w_maed_planificallegadaenca

type variables
w_mant_deta_planificallegadadeta iw_mantencion

DataWindowChild	idwc_planta, idwc_packing,idwc_clientes

Boolean lb_existe
end variables

forward prototypes
public function boolean noexistecliente (integer ai_codigo)
public subroutine habilitaingreso (string columna)
public subroutine habilitaencab (boolean habilita)
public function long buscafumigacion (integer planta)
protected function boolean wf_actualiza_db (boolean borrando)
public function boolean existendatos (integer ai_codigo)
public function boolean existeplanta (integer ai_codigo, integer ai_tipo)
end prototypes

event ue_imprimir;
OpenWithParm(w_info_planificacionllegada, istr_mant)
end event

public function boolean noexistecliente (integer ai_codigo);String	ls_nombre

SELECT	clie_nombre
	INTO	:ls_nombre  
   FROM	dbo.clientesprod  
   WHERE	clie_codigo =	:ai_codigo;
	
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura Clientes Producción")
	RETURN True
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Error", "Cliente no Existe. Ingrese otro.")
	RETURN True
END IF

RETURN False



end function

public subroutine habilitaingreso (string columna);Date	ld_fecha
Integer  li_tarjas, li_tardef
Boolean	lb_estado = True

dw_2.AcceptText()

IF dw_2.RowCount() > 0 THEN
	IF IsNull(dw_2.Object.clie_codigo[1]) OR dw_2.Object.clie_codigo[1] = 0 OR &
		IsNull(dw_2.Object.plde_codigo[1]) OR dw_2.Object.plde_codigo[1] = 0 OR &
		IsNull(dw_2.Object.plan_copack[1]) OR dw_2.Object.plan_copack[1] = 0  THEN
		lb_estado = False
	END IF
END IF

pb_ins_det.Enabled = lb_estado

IF lb_estado = True AND lb_existe = True THEN
	This.TriggerEvent("ue_recuperadatos")
END IF	

end subroutine

public subroutine habilitaencab (boolean habilita);IF Habilita THEN
	dw_2.Object.clie_codigo.Protect	=	0
	dw_2.Object.plde_codigo.Protect	=	0
	dw_2.Object.plan_copack.Protect	=	0
	dw_2.Object.plan_fechal.Protect	=	0
	dw_2.Object.plan_usuari.Protect	=	0
	dw_2.Object.plan_horall.Protect	=	0
	
	dw_2.Object.clie_codigo.Color	=	0
	dw_2.Object.plde_codigo.Color	=	0
	dw_2.Object.plan_copack.Color=	0
	dw_2.Object.plan_fechal.Color	=	0
	dw_2.Object.plan_usuari.Color	=	0
	dw_2.Object.plan_horall.Color	=	0
	
	dw_2.Object.clie_codigo.BackGround.Color	=	Rgb(255,255,255)
	dw_2.Object.plde_codigo.BackGround.Color	=	Rgb(255,255,255)
	dw_2.Object.plan_copack.BackGround.Color	=	Rgb(255,255,255)
	dw_2.Object.plan_fechal.BackGround.Color	=	Rgb(255,255,255)
	dw_2.Object.plan_usuari.BackGround.Color	=	Rgb(255,255,255)
	dw_2.Object.plan_horall.BackGround.Color	=	Rgb(255,255,255)
	
	dw_2.SetColumn("clie_codigo")
	dw_2.SetFocus()
ELSE
	dw_2.Object.clie_codigo.Protect	=	1
	dw_2.Object.plde_codigo.Protect	=	1
	dw_2.Object.plan_copack.Protect	=	1
	dw_2.Object.plan_fechal.Protect	=	1
	dw_2.Object.plan_usuari.Protect	=	1
	dw_2.Object.plan_horall.Protect	=	1
	
	dw_2.Object.clie_codigo.Color	=	Rgb(255,255,255)
	dw_2.Object.plde_codigo.Color	=	Rgb(255,255,255)
	dw_2.Object.plan_copack.Color=	Rgb(255,255,255)
	dw_2.Object.plan_fechal.Color	=	Rgb(255,255,255)
	dw_2.Object.plan_usuari.Color	=	Rgb(255,255,255)
	dw_2.Object.plan_horall.Color	=	Rgb(255,255,255)
	
	dw_2.Object.clie_codigo.BackGround.Color	=	553648127
	dw_2.Object.plde_codigo.BackGround.Color	=	553648127
	dw_2.Object.plan_copack.BackGround.Color	=	553648127
	dw_2.Object.plan_fechal.BackGround.Color	=	553648127
	dw_2.Object.plan_usuari.BackGround.Color	=	553648127
	dw_2.Object.plan_horall.BackGround.Color	=	553648127
END IF




end subroutine

public function long buscafumigacion (integer planta);Integer	li_planta, li_tipoins, li_movto
Long		ll_numero,ll_numero2, ll_fin, ll_actual

li_planta	=	planta

li_movto = 7

Select max(fumi_numero) 
Into  :ll_numero
From dbo.fumigaenc
Where plde_codigo = :li_planta;

Select como_inicia, como_actual, como_termin
Into	:ll_numero2, :ll_actual, :ll_fin
from dbo.CORRELMOVIMIENTOS 
Where plde_codigo = :li_planta
and	COMO_TIPOMV = :li_movto;

IF ll_actual >= ll_fin THEN
	Return 0
END IF	

ll_fin = ll_fin - 3

IF ll_actual >= ll_fin THEN 
	MessageBox("Advertencia","Quedan menos de 3 Correlativos, Proceda por Mantención 'Correlativos'")
END IF	

IF sqlca.SQLCode = -1 THEN
		F_errorbasedatos(sqlca,"Lectura tabla CORRELMOVIMIENTOS")
END IF

IF Isnull(ll_numero) OR String(ll_numero) = '' or ll_numero < ll_numero2 THEN
	ll_numero = ll_numero2
END IF	
	
IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla Fumigaenc")
ELSEIF sqlca.SQLCode = 0 THEN
	ll_numero++
END IF

RETURN ll_numero

end function

protected function boolean wf_actualiza_db (boolean borrando);Boolean	lb_AutoCommit, lb_Retorno
Integer li_fila, li_planta, li_movto
Long numero

IF Not dw_2.uf_check_required(0) THEN RETURN False

IF Not dw_1.uf_validate(0) THEN RETURN False

lb_AutoCommit		=	sqlca.AutoCommit
sqlca.AutoCommit	=	False

IF Borrando THEN
	For li_fila = 1 to dw_1.RowCount()
		dw_1.Object.fumi_numero[li_fila]	=	Long(istr_mant.argumento[2])
	Next
	IF dw_1.Update(True, False) = 1 THEN
		IF dw_2.Update(True, False) = 1 THEN
			Commit;
			
			IF sqlca.SQLCode <> 0 THEN
				F_ErrorBaseDatos(sqlca, This.Title)
				
				RollBack;
			ELSE
				lb_Retorno	=	True
				
				dw_1.ResetUpdate()
				dw_2.ResetUpdate()
			END IF
		ELSE
			F_ErrorBaseDatos(sqlca, This.Title)
			
			RollBack;
		END IF
	ELSE
		F_ErrorBaseDatos(sqlca, This.Title)
	END IF
ELSE
	IF dw_2.Update(True, False) = 1 THEN
		IF dw_1.Update(True, False) = 1 THEN
			Commit;
			
			IF sqlca.SQLCode <> 0 THEN
				F_ErrorBaseDatos(sqlca, This.Title)
				
				RollBack;
			ELSE
				lb_Retorno	=	True
				
				dw_1.ResetUpdate()
				dw_2.ResetUpdate()
			END IF
		ELSE
			F_ErrorBaseDatos(sqlca, This.Title)
			
			RollBack;
		END IF
	ELSE
		F_ErrorBaseDatos(sqlca, This.Title)
	END IF
END IF

sqlca.AutoCommit	=	lb_AutoCommit

RETURN lb_Retorno
end function

public function boolean existendatos (integer ai_codigo);Integer	li_cliente,li_planta, li_cont
Date		ld_fecha
Time		lt_hora

li_cliente 	= Integer(istr_mant.argumento[1])
li_planta 	= Integer(istr_mant.argumento[2])
ld_fecha	 	= date(istr_mant.argumento[4])
lt_hora		= Time(istr_mant.argumento[5])

SELECT	count(*)
	INTO	:li_cont  
   FROM	dbo.Planificallegadaenca  
   WHERE	plde_codigo =	:li_planta
	AND	clie_codigo =	:li_cliente
	AND	plan_copack =	:ai_codigo
	AND	plan_fechal =	:ld_fecha
	AND	plan_horall =	:lt_hora;
	
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura Planificallegadaenca Producción")
	RETURN False
ELSEIF li_cont = 0 THEN
	RETURN False
END IF

RETURN True

end function

public function boolean existeplanta (integer ai_codigo, integer ai_tipo);Integer	li_comt

SELECT	count(*)
	INTO	:li_comt  
   FROM	dbo.plantadesp  
   WHERE	plde_codigo =	:ai_codigo
	AND :ai_tipo in (-1,plde_tipopl);
	
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura Plantas Producción")
	RETURN True
ELSEIF li_comt = 0 THEN
	MessageBox("Error", "Planta o Packing no Existe. Ingrese otra.")
	RETURN True
END IF

RETURN False

end function

event open;
IF gi_CodExport = 300 THEN dw_2.Object.plde_codigo.dddw.Name	=	"dw_mues_plantadesp"

dw_2.GetChild("plde_codigo", idwc_planta)
idwc_planta.SetTransObject(sqlca)
idwc_planta.Retrieve(1)

dw_2.GetChild("plan_copack", idwc_packing)
idwc_packing.SetTransObject(sqlca)
idwc_packing.Retrieve()

dw_2.GetChild("clie_codigo", idwc_clientes)
idwc_clientes.SetTransObject(sqlca)
idwc_clientes.Retrieve(gi_CodExport)



call super::open

istr_mant.argumento[2]	=	String(gi_CodPlanta)
istr_mant.argumento[1]	=	String(gi_CodExport)


dw_2.Object.plan_fechal[1] = Date(Today())
dw_2.Object.plan_horall[1] = Time(Now())




end event

event ue_borra_detalle;call super::ue_borra_detalle;IF dw_1.rowcount() < 2 THEN
	dw_1.SetFocus()
	RETURN
END IF

SetPointer(HourGlass!)

ib_borrar = True
w_main.SetMicroHelp("Validando la eliminación de detalle...")

Message.DoubleParm = 0

This.TriggerEvent ("ue_validaborrar_detalle")

IF Message.DoubleParm = -1 THEN RETURN

istr_mant.borra	= True
istr_mant.agrega	= False

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
		HabilitaEncab(True)
		pb_eli_det.Enabled = False
	END IF
END IF

istr_mant.borra	 = False
end event

event ue_nuevo_detalle;call super::ue_nuevo_detalle;istr_mant.borra	= False
istr_mant.agrega	= True

OpenWithParm(iw_mantencion, istr_mant)

IF dw_1.RowCount() > 0 THEN HabilitaEncab(False)

IF dw_1.RowCount() > 0 and pb_eliminar.Enabled = FALSE THEN
	pb_eliminar.Enabled	= TRUE
	pb_grabar.Enabled		= TRUE
END IF
	
dw_1.SetRow(il_fila)
dw_1.SelectRow(il_fila,True)

end event

event ue_recuperadatos;call super::ue_recuperadatos;Long	ll_fila_d, ll_fila_e, respuesta
DO
	dw_2.SetRedraw(False)
	dw_2.Reset()
	ll_fila_e	= dw_2.Retrieve(integer(istr_mant.argumento[1]),integer(istr_mant.argumento[2]),integer(istr_mant.argumento[3]),&
					date(istr_mant.argumento[4]),time(istr_mant.argumento[5]))

	IF ll_fila_e = -1 THEN
		respuesta = MessageBox(	"Error en Base de Datos", "No es posible conectar la Base de Datos.", &
										Information!, RetryCancel!)
	ELSE
		DO
			ll_fila_d	= dw_1.Retrieve(integer(istr_mant.argumento[1]),integer(istr_mant.argumento[2]),integer(istr_mant.argumento[3]),&
						date(istr_mant.argumento[4]),time(istr_mant.argumento[5]))

			IF ll_fila_d = -1 THEN
				respuesta = MessageBox(	"Error en Base de Datos", "No es posible conectar la Base de Datos.", &
												Information!, RetryCancel!)
			ELSE
				pb_eliminar.Enabled	= True
				pb_grabar.Enabled		= True
				pb_imprimir.Enabled	= True
				pb_ins_det.Enabled	= True
				
				IF ll_fila_e > 0 THEN
				
					IF ll_fila_d > 0 THEN
						pb_eli_det.Enabled	= True
						dw_1.SetRow(1)
						dw_1.SelectRow(1,True)
						dw_1.SetFocus()
						HabilitaEncab(False)
					ELSE
						//pb_nuevo.PostEvent(Clicked!)
						pb_ins_det.SetFocus()
					END IF
				END IF	
			END IF
		LOOP WHILE respuesta = 1

		IF respuesta = 2 THEN Close(This)
	END IF
	dw_2.SetRedraw(True)
LOOP WHILE respuesta = 1

IF respuesta = 2 THEN Close(This)


end event

on w_maed_planificallegadaenca.create
call super::create
end on

on w_maed_planificallegadaenca.destroy
call super::destroy
end on

event ue_nuevo;Long		ll_modif1, ll_modif2

ib_ok	= True

istr_mant.Solo_Consulta	=	istr_mant.UsuarioSoloConsulta

IF Not istr_mant.Solo_Consulta THEN
	CHOOSE CASE wf_modifica()
		CASE -1
			ib_ok = False
		CASE 0
			ll_modif1	=	dw_1.GetNextModified(0, Primary!)
			ll_modif2	=	dw_2.GetNextModified(0, Primary!)
		
//			IF ib_ModEncab OR dw_1.GetNextModified(0, Primary!) > 0 THEN
			IF dw_1.RowCount() > 0 THEN
				CHOOSE CASE MessageBox("Grabar registro(s)","Desea Grabar la información ?", Question!, YesNoCancel!)
					CASE 1
						Message.DoubleParm = 0
						This.TriggerEvent("ue_guardar")
						IF message.DoubleParm = -1 THEN ib_ok = False
					CASE 3
						ib_ok	= False
						RETURN
				END CHOOSE
			END IF
	END CHOOSE
END IF

IF Not ib_ok THEN RETURN

dw_1.Reset()

pb_eli_det.Enabled		=	False
pb_ins_det.Enabled		=	False
pb_grabar.Enabled			=	False
pb_eliminar.Enabled		=	False
pb_imprimir.Enabled		=	False
dw_2.Enabled				=	True

dw_2.SetRedraw(False)
dw_2.Reset()
dw_2.InsertRow(0)
dw_2.SetRedraw(True)

dw_2.SetFocus()

HabilitaEncab(True)

dw_2.SetItem(1, "clie_codigo", gi_CodExport)
dw_2.SetItem(1, "plde_codigo", gi_CodPlanta)
dw_2.Object.plan_fechal[1] = Date(Today())
dw_2.Object.plan_horall[1] = Time(Now())
dw_2.Object.plan_usuari[1] = gstr_us.Nombre

istr_mant.argumento[2]	=	String(gi_CodPlanta)
istr_mant.argumento[1]	=	String(gi_CodExport)
istr_mant.argumento[4]  =  string( dw_2.Object.plan_fechal[1])
istr_mant.argumento[5]  =  string( dw_2.Object.plan_horall[1])




end event

event ue_seleccion;Long		ll_null

SetNull(ll_null)

istr_busq.argum[1]	=	String(dw_2.Object.clie_codigo[1])
istr_busq.argum[2]	=	String(dw_2.Object.plde_codigo[1])

OpenWithParm(w_busc_planificallegada, istr_busq)

istr_busq	=	Message.PowerObjectParm

IF istr_busq.argum[4] <> "" THEN

	istr_mant.argumento[3]  = istr_busq.argum[3]
	istr_mant.argumento[4]  = istr_busq.argum[4]
	istr_mant.argumento[5]  = istr_busq.argum[5]
	
	This.TriggerEvent("ue_recuperadatos")
	
ELSE
	pb_buscar.SetFocus()
END IF
end event

event ue_modifica_detalle;IF dw_1.RowCount() > 0 THEN
	istr_mant.agrega			= False
	istr_mant.borra			= False
	
	OpenWithParm(iw_mantencion, istr_mant)
	
END IF
end event

event ue_guardar;Integer  li_fila

IF dw_1.AcceptText() = -1 THEN RETURN

SetPointer(HourGlass!)

w_main.SetMicroHelp("Grabando información...")

Message.DoubleParm = 0

TriggerEvent("ue_antesguardar")

IF Message.DoubleParm = -1 THEN RETURN

IF wf_actualiza_db(False) THEN
	w_main.SetMicroHelp("Información Grabada.")
	pb_eliminar.Enabled	= True
	pb_imprimir.Enabled	= True
ELSE
	w_main.SetMicroHelp("No se puede Grabar información.")
	Message.DoubleParm = -1
	RETURN
END IF
end event

type dw_1 from w_mant_encab_deta_csd`dw_1 within w_maed_planificallegadaenca
integer x = 32
integer y = 684
integer width = 2779
integer height = 1128
integer taborder = 100
string title = "Detalle de Pallets"
string dataobject = "dw_mues_planificallegadadeta"
boolean livescroll = false
end type

event dw_1::retrieveend;If RowCount() > 0 Then	pb_ins_det.Enabled = TRUE
end event

type dw_2 from w_mant_encab_deta_csd`dw_2 within w_maed_planificallegadaenca
integer x = 667
integer y = 68
integer width = 1568
integer height = 536
string dataobject = "dw_mant_planificallegadaenca"
end type

event dw_2::itemchanged;call super::itemchanged;Integer	li_Planta
Long		ll_null
String	ls_columna
Date		ld_nula

SetNull(ld_nula)
SetNull(ll_null)
ls_columna = dwo.Name
 
CHOOSE CASE ls_columna
	CASE "clie_codigo"
		IF NoExisteCliente(Integer(data)) THEN
			This.SetItem(Row, ls_Columna, Integer(ll_null))
			RETURN 1
		END IF	
		
		istr_mant.argumento[1] = data

	CASE "plde_codigo"
		IF existeplanta(Integer(data),1) THEN
			This.SetItem(Row, ls_Columna, Integer(ll_null))
			RETURN 1
		END IF			
		li_Planta	=	Integer(data)
		istr_mant.Argumento[2]	=	String(data)
		
	CASE "plan_copack"
		IF existeplanta(Integer(data),-1) THEN
			This.SetItem(Row, ls_Columna, Integer(ll_null))
			RETURN 1
		ELSE	
			lb_existe = existendatos(Integer(data))
		END IF	
		
		istr_mant.Argumento[3]	=	String(data)	
		
	CASE "plan_fechal"
		istr_mant.Argumento[4]	=	String(data)	
		
	CASE "plan_horall"
		istr_mant.Argumento[5]	=	String(data)		
		
END CHOOSE

HabilitaIngreso(ls_columna)
end event

event dw_2::clicked;call super::clicked;dw_2.AcceptText()
end event

type pb_nuevo from w_mant_encab_deta_csd`pb_nuevo within w_maed_planificallegadaenca
integer x = 3145
integer y = 296
end type

type pb_eliminar from w_mant_encab_deta_csd`pb_eliminar within w_maed_planificallegadaenca
integer x = 3145
integer y = 528
end type

type pb_grabar from w_mant_encab_deta_csd`pb_grabar within w_maed_planificallegadaenca
integer x = 3145
integer y = 740
end type

type pb_imprimir from w_mant_encab_deta_csd`pb_imprimir within w_maed_planificallegadaenca
integer x = 3145
integer y = 972
boolean enabled = true
boolean default = true
end type

type pb_salir from w_mant_encab_deta_csd`pb_salir within w_maed_planificallegadaenca
integer x = 3150
integer y = 1668
end type

type pb_ins_det from w_mant_encab_deta_csd`pb_ins_det within w_maed_planificallegadaenca
integer x = 3145
integer y = 1360
end type

type pb_eli_det from w_mant_encab_deta_csd`pb_eli_det within w_maed_planificallegadaenca
integer x = 3145
integer y = 1528
end type

type pb_buscar from w_mant_encab_deta_csd`pb_buscar within w_maed_planificallegadaenca
integer x = 3145
integer y = 116
end type

