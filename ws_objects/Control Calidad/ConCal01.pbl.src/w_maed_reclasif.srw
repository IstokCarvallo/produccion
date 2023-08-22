$PBExportHeader$w_maed_reclasif.srw
$PBExportComments$Ventana Mantenedor Encabezado de Reetiquetado.
forward
global type w_maed_reclasif from w_mant_encab_deta_csd
end type
end forward

global type w_maed_reclasif from w_mant_encab_deta_csd
integer width = 3488
integer height = 2568
string title = "MANTENCION DE RECLASIFICACIONES"
string menuname = ""
boolean maxbox = false
event ue_imprimir ( )
end type
global w_maed_reclasif w_maed_reclasif

type variables
w_mant_deta_reclasif iw_mantencion

DataWindowChild	dw_planta, dw_clientes, idwc_plta
						
end variables

forward prototypes
public function boolean noexistecliente (integer ai_codigo)
public subroutine habilitaingreso (string columna)
public subroutine habilitaencab (boolean habilita)
public function boolean noexisteplanta (integer ai_cli, integer ai_planta)
protected function boolean existereclasif (long al_numero)
end prototypes

event ue_imprimir;SetPointer(HourGlass!)

Long		fila
str_info	lstr_info

lstr_info.titulo	= "INFORME RECLASIFICACION"
lstr_info.copias	= 1

OpenWithParm(vinf,lstr_info)

vinf.dw_1.DataObject = "dw_info_reclasificacion"

vinf.dw_1.SetTransObject(sqlca)

vinf.dw_1.GetChild("plde_codigo", idwc_plta)
idwc_plta.SetTransObject(sqlca)
idwc_plta.Retrieve(Integer(istr_mant.argumento[3]))

fila = vinf.dw_1.Retrieve(dw_2.Object.clie_codigo[1], &
									dw_2.Object.plde_codigo[1], &
									dw_2.Object.recl_numero[1])

IF fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ELSEIF fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", &
					StopSign!, Ok!)
ELSE
	F_Membrete(vinf.dw_1)
	IF gs_Ambiente <> 'Windows' THEN
		F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)

	END IF
END IF

SetPointer(Arrow!)
end event

public function boolean noexistecliente (integer ai_codigo);integer	li_existe
boolean  lb_retorno 

SELECT	count(*)
	INTO	:li_existe  
   FROM	dbo.clientesprod  
   WHERE	clie_codigo =	:ai_codigo;
	
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura Clientes Producción")
	lb_retorno =true
ELSEIF li_existe = 0 THEN
	lb_retorno =true
	MessageBox("Error", "Cliente no Existe. Ingrese otro.")	
END IF

RETURN lb_retorno
//IF F_ValidaCliente(ai_codigo) THEN
//	istr_mant.Argumento[3]	=	String(ai_codigo)
//	RETURN False
//ELSE
//	RETURN True
//END IF
end function

public subroutine habilitaingreso (string columna);Date		ld_fecha


dw_2.AcceptText()

IF dw_2.RowCount() > 0 THEN
	IF IsNull(dw_2.Object.plde_codigo[1]) OR dw_2.Object.plde_codigo[1] = 0 OR &
		IsNull(dw_2.Object.recl_numero[1]) OR dw_2.Object.recl_numero[1] = 0 OR &
		IsNull(dw_2.Object.recl_fereet[1]) OR dw_2.Object.recl_fereet[1] = ld_fecha THEN
		
	END IF
END IF


end subroutine

public subroutine habilitaencab (boolean habilita);IF Habilita THEN
	dw_2.SetTabOrder("clie_codigo",10)
	dw_2.SetTabOrder("plde_codigo",20)
	dw_2.SetTabOrder("recl_numero",30)
	dw_2.SetTabOrder("recl_fereet",40)
	dw_2.SetTabOrder("recl_observ",50)
	dw_2.Modify("clie_codigo.BackGround.Color = " + String(RGB(255,255,255)))
	dw_2.Modify("plde_codigo.BackGround.Color = " + String(RGB(255,255,255)))
	dw_2.Modify("recl_numero.BackGround.Color = " + String(RGB(255,255,255)))
	dw_2.Modify("recl_fereet.BackGround.Color = " + String(RGB(255,255,255)))
	dw_2.Modify("recl_observ.BackGround.Color = " + String(RGB(255,255,255)))
	dw_2.SetColumn("recl_numero")
	dw_2.SetFocus()
ELSE
	dw_2.SetTabOrder("clie_codigo",0)
	dw_2.SetTabOrder("plde_codigo",0)
	dw_2.SetTabOrder("recl_numero",0)
	dw_2.SetTabOrder("recl_fereet",0)
	dw_2.SetTabOrder("recl_observ",0)
	dw_2.Modify("clie_codigo.BackGround.Color = " + String(RGB(192,192,192)))
	dw_2.Modify("plde_codigo.BackGround.Color = " + String(RGB(192,192,192)))
	dw_2.Modify("recl_numero.BackGround.Color = " + String(RGB(192,192,192)))
	dw_2.Modify("recl_fereet.BackGround.Color = " + String(RGB(192,192,192)))
	dw_2.Modify("recl_observ.BackGround.Color = " + String(RGB(192,192,192)))
END IF
end subroutine

public function boolean noexisteplanta (integer ai_cli, integer ai_planta);integer	li_existe
boolean  lb_retorno 

SELECT	count(*)
	INTO	:li_existe  
   FROM	dbo.plantadesp  
   WHERE	plde_codigo = :ai_planta;	
	
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura Plantas")
	lb_retorno =true
ELSEIF li_existe = 0 THEN
	lb_retorno =true
	MessageBox("Error", "No Existe Planta. Ingrese otra.")	
END IF

RETURN lb_retorno
end function

protected function boolean existereclasif (long al_numero);Integer	li_Planta, li_Existe, li_Cliente, li_null
Long		ll_Numero

setNull(li_null)

li_Cliente	=	dw_2.Object.clie_codigo[1]
li_Planta	=	dw_2.Object.plde_codigo[1]

SELECT	Count(*) 
	INTO	:li_Existe
	FROM	dbo.RECLASIFENC
	WHERE	recl_numero	=	:al_numero
	AND	clie_codigo =  :li_Cliente
	AND	plde_codigo	=	:li_Planta;
				
IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla Reclasifenc")
	RETURN True
ELSE
	IF li_Existe > 0 THEN
		istr_mant.argumento[1]	=	String(li_Planta)
		istr_mant.argumento[2]	=	String(al_numero)
		istr_mant.argumento[3]	=	String(li_cliente)
		
		This.TriggerEvent("ue_recuperadatos")
		
		RETURN False
	ELSE
		MessageBox("Error", "No Existe Reclasificacion", Exclamation!)
		istr_mant.argumento[2]	=	String(al_numero)
		istr_mant.argumento[3]	=	String(li_Cliente)
		istr_mant.argumento[1]	=	String(li_Planta)
		RETURN true
	END IF   
END IF
end function

event open;call super::open;/*
  istr_mant.argumento[1]	= Planta
  istr_mant.argumento[2]	= Numero Reclasificacion
  istr_mant.argumento[3]	= Cliente
*/
				
dw_2.GetChild("clie_codigo", dw_clientes)
dw_clientes.SetTransObject(sqlca)
dw_clientes.Retrieve(gi_CodExport)

dw_2.GetChild("plde_codigo", dw_planta)
dw_planta.SetTransObject(sqlca)
dw_planta.Retrieve(gi_CodExport,1)

buscar	= "Pallet Nuevo:Npaen_numero,Variedad:Svari_nombre"
ordenar	= "Pallet Nuevo:paen_numero,Variedad:vari_nombre"

end event

event ue_borra_detalle;call super::ue_borra_detalle;IF dw_1.rowcount() = 0 THEN
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

event ue_nuevo_detalle;istr_mant.borra	= False
istr_mant.agrega	= True

OpenWithParm(iw_mantencion, istr_mant)

IF dw_1.RowCount() > 0 THEN HabilitaEncab(False)

	IF dw_1.RowCount() > 0 and pb_eliminar.Enabled = FALSE THEN
		pb_eliminar.Enabled	= TRUE
		pb_grabar.Enabled		= TRUE
		pb_eli_det.enabled   = TRUE
	END IF

dw_1.SetRow(il_fila)
dw_1.SelectRow(il_fila,True)
end event

event ue_recuperadatos;call super::ue_recuperadatos;Long	ll_fila_d, ll_fila_e, respuesta
DO
	dw_2.SetRedraw(False)
	dw_2.Reset()
	ll_fila_e	= dw_2.Retrieve(Long(istr_mant.argumento[2]), Integer(istr_mant.argumento[3]),Integer(istr_mant.argumento[1]))

	IF ll_fila_e = -1 THEN
		respuesta = MessageBox(	"Error en Base de Datos", "No es posible conectar la Base de Datos.", &
										Information!, RetryCancel!)
	ELSE
		DO
			ll_fila_d	= dw_1.Retrieve(Long(istr_mant.argumento[2]), Integer(istr_mant.argumento[3]), Integer(istr_mant.argumento[1]))

			IF ll_fila_d = -1 THEN
				respuesta = MessageBox(	"Error en Base de Datos", "No es posible conectar la Base de Datos.", &
												Information!, RetryCancel!)
			ELSE
				pb_eliminar.Enabled	= True
				pb_grabar.Enabled		= True
				pb_imprimir.Enabled		= True
				pb_ins_det.Enabled	= True

				IF ll_fila_d > 0 THEN
				   pb_eli_det.Enabled	= True
					dw_1.SetRow(1)
					dw_1.SelectRow(1,True)
					dw_1.SetFocus()
					HabilitaEncab(False)
				ELSE
					pb_ins_det.SetFocus()
				END IF
			END IF
		LOOP WHILE respuesta = 1

		IF respuesta = 2 THEN Close(This)
	END IF
	dw_2.SetRedraw(True)
LOOP WHILE respuesta = 1

IF respuesta = 2 THEN Close(This)

dw_2.Enabled	=	False
end event

on w_maed_reclasif.create
call super::create
end on

on w_maed_reclasif.destroy
call super::destroy
end on

event ue_nuevo;call super::ue_nuevo;HabilitaEncab(True)

dw_2.SetItem(1, "clie_codigo", gi_CodExport)
dw_2.SetItem(1, "plde_codigo", gi_CodPlanta)
dw_2.setitem(1, "recl_fereet", today())
pb_ins_det.enabled = TRUE
 
istr_mant.argumento[3]	=	String(gi_CodExport)
istr_mant.argumento[1]	=	String(gi_CodPlanta)






end event

event ue_seleccion;call super::ue_seleccion;Long		ll_null

SetNull(ll_null)

istr_busq.argum[1]	=	String(dw_2.Object.clie_codigo[1])
istr_busq.argum[2]   = ""
OpenWithParm(w_busc_reclasif, istr_busq)

istr_busq	=	Message.PowerObjectParm

IF istr_busq.argum[2] <> "" THEN
	istr_mant.argumento[1]	= istr_busq.argum[1]
	istr_mant.argumento[2]	= istr_busq.argum[2]
	istr_mant.argumento[3]  = istr_busq.argum[3]
	dw_2.SetItem(1, "recl_numero", Long(istr_mant.argumento[2]))
	dw_2.SetFocus()
	dw_2.SetColumn("recl_numero")	
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

event ue_antesguardar;call super::ue_antesguardar;Integer	li_Secuen
Long 		ll_Fila, ll_Numero
Integer  li_Cliente, li_Planta 

IF dw_1.rowcount()	>	0 THEN 
	
	li_Cliente	=	Integer(istr_mant.argumento[3])
	li_Planta	=	Integer(istr_mant.argumento[1])
	ll_Numero	=	Long(istr_mant.argumento[2])
	
	SetNull(ll_Numero)
	
	IF dw_2.GetItemStatus(1, 0, Primary!) = NewModified! then 
		/*
		Se actualiza tabla de Encabezado, a objeto de bloquearla hasta que termine la
		Grabación de Nota Venta
		*/
		
		SELECT	IsNull(Max(recl_numero),0) + 1
			INTO	:ll_Numero  
			FROM	dba.reclasifenc 
			WHERE clie_codigo = :li_Cliente
			AND   plde_codigo = :li_Planta;	
		
//		IF IsNull(li_numero) THEN
//			li_numero = 1
//		ELSE
//			li_Numero++
//		END IF
		
		IF sqlca.sqlcode = -1 THEN
			F_ErrorBaseDatos(sqlca,"No se pudo obtener Número Máximo")
		END IF
	ELSE
		ll_Numero = dw_2.Object.recl_numero[1]
	END IF
	
	IF ll_Numero > 0 THEN	
		
		dw_2.SetItem(1,"recl_numero",ll_Numero)
				
		/*
		Deja tomada la transacción hasta el próximo commit.
		Así pueden guardar más de una persona a la vez.
		*/
		
		SELECT	IsNull(Max(recl_secuen), 0)
			INTO	:li_Secuen
			FROM	dba.reclasifdet
			WHERE  clie_codigo = :li_Cliente
			AND    plde_codigo = :li_Planta
			AND    recl_numero = :ll_Numero;	
			
		FOR ll_Fila = 1 TO dw_1.RowCount()
			IF dw_1.GetItemStatus(ll_Fila, 0, Primary!) = NewModified! THEN
				li_Secuen ++
				dw_1.Object.recl_secuen[ll_Fila] = li_Secuen
				dw_1.Object.recl_numero[ll_Fila] = ll_Numero
				dw_1.Object.recl_horare[ll_Fila] = Now()				
			END IF
		NEXT
	END IF
ELSE 
	w_main.SetMicroHelp("No se puede Grabar información.")
	Message.DoubleParm = -1
	RETURN
END IF 
end event

type dw_1 from w_mant_encab_deta_csd`dw_1 within w_maed_reclasif
integer x = 41
integer y = 604
integer width = 2885
integer height = 1808
integer taborder = 100
string title = "Detalle de Reclasificaciones"
string dataobject = "dw_mues_reclasif"
boolean livescroll = false
end type

event dw_1::retrieveend;If RowCount() > 0 Then	pb_ins_det.Enabled = TRUE
end event

type dw_2 from w_mant_encab_deta_csd`dw_2 within w_maed_reclasif
integer x = 169
integer y = 68
integer width = 2546
integer height = 432
string title = "Mantencion Reclasificacion"
string dataobject = "dw_mant_reclasif"
end type

event dw_2::itemchanged;call super::itemchanged;Long		ll_null
String	ls_columna
Date		ld_nula

SetNull(ll_null)
SetNull(ld_nula)
ls_columna = dwo.Name

CHOOSE CASE ls_columna

	CASE "clie_codigo"
		IF NoExisteCliente(Integer(data)) THEN
			This.SetItem(Row, ls_Columna, gi_codexport)
			RETURN 1
		ELSE
			istr_mant.argumento[3] = data
         dw_2.GetChild("plde_codigo", dw_planta)
			dw_planta.SetTransObject(sqlca)
			dw_planta.Retrieve(Integer(istr_mant.argumento[3]),1)
		END IF

	CASE "plde_codigo"
		IF NoExistePlanta(dw_2.object.clie_codigo[1],Integer(data)) THEN
			This.SetItem(Row, ls_Columna, ll_null)
			RETURN 1
		ELSE
			istr_mant.argumento[1] = data
		END IF

	CASE "recl_numero"
      IF ExisteReclasif(Long(data)) THEN
			This.SetItem(Row, ls_Columna, ll_null)
			RETURN 1
		END IF

	CASE "recl_fereet"
		istr_mant.argumento[5] = data
END CHOOSE

HabilitaIngreso(ls_columna)
end event

event dw_2::doubleclicked;//
end event

type pb_nuevo from w_mant_encab_deta_csd`pb_nuevo within w_maed_reclasif
integer x = 3090
integer y = 332
end type

type pb_eliminar from w_mant_encab_deta_csd`pb_eliminar within w_maed_reclasif
integer x = 3090
integer y = 560
end type

type pb_grabar from w_mant_encab_deta_csd`pb_grabar within w_maed_reclasif
integer x = 3090
integer y = 788
end type

type pb_imprimir from w_mant_encab_deta_csd`pb_imprimir within w_maed_reclasif
integer x = 3090
integer y = 1016
boolean enabled = true
boolean default = true
end type

type pb_salir from w_mant_encab_deta_csd`pb_salir within w_maed_reclasif
integer x = 3090
integer y = 1244
end type

type pb_ins_det from w_mant_encab_deta_csd`pb_ins_det within w_maed_reclasif
integer x = 3090
integer y = 1584
boolean enabled = true
end type

type pb_eli_det from w_mant_encab_deta_csd`pb_eli_det within w_maed_reclasif
integer x = 3090
integer y = 1800
end type

type pb_buscar from w_mant_encab_deta_csd`pb_buscar within w_maed_reclasif
integer x = 3090
integer y = 104
end type

