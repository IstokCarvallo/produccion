$PBExportHeader$w_maed_movtoserviadienca.srw
forward
global type w_maed_movtoserviadienca from w_mant_encab_deta_csd
end type
type dw_3 from datawindow within w_maed_movtoserviadienca
end type
end forward

global type w_maed_movtoserviadienca from w_mant_encab_deta_csd
integer width = 3785
integer height = 2064
string title = "MANTENCION DE SERVICIOS ADICIONALES"
string menuname = ""
boolean maxbox = false
event ue_imprimir ( )
dw_3 dw_3
end type
global w_maed_movtoserviadienca w_maed_movtoserviadienca

type variables
w_mant_deta_movtoserviadideta iw_mantencion

DataWindowChild	idwc_planta, idwc_cliente, idwc_servicio, idwc_detalle
end variables

forward prototypes
public function boolean noexistecliente (integer ai_codigo)
public subroutine habilitaingreso (string columna)
public subroutine habilitaencab (boolean habilita)
protected function boolean wf_actualiza_db (boolean borrando)
public function boolean existe_planta (integer ai_codigo)
public function boolean noexisteservicios (string columna, integer ai_codigo)
public function long existefolio (integer ai_cliente, integer ai_planta)
public function boolean existeregistro (integer ai_cliente, integer ai_planta, long al_numero)
end prototypes

event ue_imprimir;SetPointer(HourGlass!)

Long		fila
str_info	lstr_info

lstr_info.titulo	= "MOVIMIENTOS SERVICIOS ADICIONALES"
lstr_info.copias	= 1

OpenWithParm(vinf,lstr_info)

vinf.dw_1.DataObject = "dw_info_movtoservimovtos"

vinf.dw_1.SetTransObject(sqlca)

fila = vinf.dw_1.Retrieve(Integer(istr_mant.argumento[2]), integer(istr_mant.argumento[1]),&
								Long(istr_mant.argumento[3]))

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

public function boolean noexistecliente (integer ai_codigo);String	ls_nombre

SELECT	clie_nombre
	INTO	:ls_nombre  
   FROM	dba.clientesprod  
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

public subroutine habilitaingreso (string columna);Boolean	lb_estado = True

dw_2.AcceptText()

IF dw_2.RowCount() > 0 THEN
	IF IsNull(dw_2.Object.plde_codigo[1]) OR dw_2.Object.plde_codigo[1] = 0 OR &
		IsNull(dw_2.Object.clie_codigo[1]) OR dw_2.Object.clie_codigo[1] = 0 OR &
		IsNull(dw_2.Object.sere_codigo[1]) OR dw_2.Object.sere_codigo[1] = 0 OR &
		IsNull(dw_2.Object.serd_codigo[1]) OR dw_2.Object.serd_codigo[1] = 0 THEN
		lb_estado = False
	END IF
END IF

pb_ins_det.Enabled = lb_estado
end subroutine

public subroutine habilitaencab (boolean habilita);IF Habilita THEN
	dw_2.SetTabOrder("clie_codigo",10)
	dw_2.SetTabOrder("plde_codigo",20)
	dw_2.SetTabOrder("mose_numero",30)
	dw_2.SetTabOrder("sere_codigo",40)
	dw_2.SetTabOrder("serd_codigo",50)	
	dw_2.SetTabOrder("mose_fecmov",60)
	dw_2.SetTabOrder("mose_hormov",70)	
	dw_2.Modify("mose_numero.BackGround.Color = " + String(rgb(255,255,255)))
	dw_2.Modify("clie_codigo.BackGround.Color = " + String(rgb(255,255,255)))
	dw_2.Modify("plde_codigo.BackGround.Color = " + String(rgb(255,255,255)))
	dw_2.Modify("sere_codigo.BackGround.Color = " + String(rgb(255,255,255)))
	dw_2.Modify("serd_codigo.BackGround.Color = " + String(rgb(255,255,255)))
	dw_2.Modify("mose_fecmov.BackGround.Color = " + String(rgb(255,255,255)))
	dw_2.Modify("mose_hormov.BackGround.Color = " + String(rgb(255,255,255)))
	dw_2.SetColumn("mose_numero")
	dw_2.SetFocus()
ELSE
	dw_2.SetTabOrder("mose_numero",0)
	dw_2.SetTabOrder("clie_codigo",0)
	dw_2.SetTabOrder("plde_codigo",0)
	dw_2.SetTabOrder("sere_codigo",0)
	dw_2.SetTabOrder("serd_codigo",0)
	dw_2.SetTabOrder("mose_fecmov",0)
	dw_2.SetTabOrder("mose_hormov",0)	
	dw_2.Modify("mose_numero.BackGround.Color = " + String(RGB(166,180,210)))
	dw_2.Modify("clie_codigo.BackGround.Color = " + String(RGB(166,180,210)))
	dw_2.Modify("plde_codigo.BackGround.Color = " + String(RGB(166,180,210)))
	dw_2.Modify("sere_codigo.BackGround.Color = " + String(RGB(166,180,210)))
	dw_2.Modify("serd_codigo.BackGround.Color = " + String(RGB(166,180,210)))	
	dw_2.Modify("mose_fecmov.BackGround.Color = " + String(rgb(166,180,210)))
	dw_2.Modify("mose_hormov.BackGround.Color = " + String(rgb(166,180,210)))
END IF
end subroutine

protected function boolean wf_actualiza_db (boolean borrando);Boolean	lb_AutoCommit, lb_Retorno
Integer li_fila, li_planta, li_movto, li_cliente
Long numero, ll_fila, ll_numero, ll_pallet

IF Not dw_2.uf_check_required(0) THEN RETURN False

IF Not dw_1.uf_validate(0) THEN RETURN False

lb_AutoCommit		=	sqlca.AutoCommit
sqlca.AutoCommit	=	False

IF Borrando THEN
	IF dw_1.Update(True, False) = 1 THEN
		IF dw_2.Update(True, False) = 1 THEN
			IF dw_3.Update(True, False) = 1 THEN
				Commit;
				
				IF sqlca.SQLCode <> 0 THEN
					F_ErrorBaseDatos(sqlca, This.Title)
					
					RollBack;
				ELSE
					lb_Retorno	=	True
					
					dw_1.ResetUpdate()
					dw_2.ResetUpdate()
					dw_3.ResetUpdate()
				END IF
			END IF	
		ELSE
			F_ErrorBaseDatos(sqlca, This.Title)
			
			RollBack;
		END IF
	ELSE
		F_ErrorBaseDatos(sqlca, This.Title)
	END IF
ELSE
	
	FOR ll_fila = 1 TO dw_3.RowCount()
		li_cliente = dw_3.Object.clie_codigo[ll_fila]
		li_planta = dw_3.Object.plde_codigo[ll_fila]
		ll_numero = dw_3.Object.mose_numero[ll_fila]
		ll_pallet = dw_3.Object.paen_numero[ll_fila]
		
		DELETE dba.movtoserviadicaracteristicas
		WHERE clie_codigo = :li_cliente
		AND plde_codigo = :li_planta
		AND paen_numero = :ll_pallet
		AND mose_numero = :ll_numero;
		COMMIT;
	NEXT	
		
	IF dw_2.Update(True, False) = 1 THEN
		IF dw_1.Update(True, False) = 1 THEN
			IF dw_3.Update(True, False) = 1 THEN
				Commit;
				
				IF sqlca.SQLCode <> 0 THEN
					F_ErrorBaseDatos(sqlca, This.Title)
					
					RollBack;
				ELSE
					lb_Retorno	=	True
					
					dw_1.ResetUpdate()
					dw_2.ResetUpdate()
					dw_3.ResetUpdate()
				END IF
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

public function boolean existe_planta (integer ai_codigo);Integer	ll_cont

SELECT	count()
	INTO	:ll_cont
   FROM	dba.plantadesp 
   WHERE	plde_codigo =	:ai_codigo;
	
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura Planta Producción")
	RETURN True
ELSEIF ll_cont = 0 THEN
	MessageBox("Error", "Planta no Existe. Ingrese otra.")
	RETURN True
END IF

RETURN False

end function

public function boolean noexisteservicios (string columna, integer ai_codigo);String	ls_nombre
Integer	li_planta, cont

li_planta = Integer(istr_mant.argumento[1])

IF columna = 'sere_codigo' THEN
  SELECT count()  
    INTO :cont
    FROM dba.serviplantaenca  
   WHERE plde_codigo = :li_planta
	AND sere_codigo = :ai_codigo ;
	
	
	IF sqlca.SQLCode = -1 THEN
		F_ErrorBaseDatos(sqlca, "Lectura Tabla Servicios encabezado")
		RETURN True
	ELSEIF cont = 0 THEN
		MessageBox("Error", "Servicio no Existe. Ingrese otro.")
		RETURN True
	ELSE
		RETURN False
	END IF
ELSE
	 SELECT count()  
    INTO :cont
    FROM dba.serviplantadeta 
   WHERE plde_codigo = :li_planta
	AND sere_codigo = :ai_codigo ;
		
	IF sqlca.SQLCode = -1 THEN
		F_ErrorBaseDatos(sqlca, "Lectura Tabla Servicios detalle")
		RETURN True
	ELSEIF cont = 0 THEN
		MessageBox("Error", "Servicio Detalle no Existe. Ingrese otro.")
		RETURN True
	ELSE
		RETURN False
	END IF
	
	
END IF	




end function

public function long existefolio (integer ai_cliente, integer ai_planta);Long	ll_cont

SELECT	count()
	INTO	:ll_cont  
   FROM	dba.movtoserviadienca  
   WHERE	clie_codigo =	:ai_cliente
	AND	plde_codigo =  :ai_planta;
	
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura Clientes Producción")
	RETURN 0
ELSEIF ll_cont = 0 THEN
	ll_cont = 1
ELSE
	ll_cont = ll_cont + 1
END IF

RETURN ll_cont

end function

public function boolean existeregistro (integer ai_cliente, integer ai_planta, long al_numero);Integer	li_cont

SELECT	count()
	INTO	:li_cont  
   FROM	dba.movtoserviadienca  
   WHERE	clie_codigo =	:ai_cliente
	AND	plde_codigo =  :ai_planta
	AND	mose_numero =	:al_numero;
	
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura Clientes Producción")
	RETURN True
ELSEIF li_cont = 0 THEN
	Return False
ELSE
	Return True
END IF

RETURN False

end function

event open;/*
  istr_mant.argumento[1]	= li_planta
  istr_mant.argumento[2]	= Cliente
*/

dw_2.GetChild("plde_codigo", idwc_planta)
idwc_planta.SetTransObject(sqlca)
idwc_planta.Retrieve(1)
dw_2.Object.plde_codigo[1] = gi_CodPlanta

dw_2.GetChild("clie_codigo", idwc_cliente)
idwc_cliente.SetTransObject(sqlca)
idwc_cliente.Retrieve(1)
dw_2.Object.clie_codigo[1] = gi_CodExport

dw_2.GetChild("sere_codigo", idwc_servicio)
idwc_servicio.SetTransObject(sqlca)
idwc_servicio.Retrieve(Integer(gi_CodPlanta))

call super::open

dw_3.SetTransObject(sqlca)

istr_mant.dw2						=	dw_3

istr_mant.argumento[1]	=	String(gi_CodPlanta)
istr_mant.argumento[2]	=	String(gi_CodExport)
dw_2.Object.mose_fecmov[1] = Date(Today())
dw_2.Object.mose_hormov[1] = Time(Now())
dw_2.Object.mose_digita[1] = gstr_us.Nombre

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

istr_mant.argumento[1] = String(dw_2.Object.plde_codigo[1])
istr_mant.argumento[2] = String(dw_2.Object.clie_codigo[1])

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
	ll_fila_e	= dw_2.Retrieve(Integer(istr_mant.argumento[1]), Long(istr_mant.argumento[2]),Long(istr_mant.argumento[3]))

	IF ll_fila_e = -1 THEN
		respuesta = MessageBox(	"Error en Base de Datos", "No es posible conectar la Base de Datos.", &
										Information!, RetryCancel!)
									
	ELSE
		DO
						
			ll_fila_d	= dw_1.Retrieve(Integer(istr_mant.argumento[1]), Long(istr_mant.argumento[2]), Long(istr_mant.argumento[3]))

			IF ll_fila_d = -1 THEN
				respuesta = MessageBox(	"Error en Base de Datos", "No es posible conectar la Base de Datos.", &
												Information!, RetryCancel!)
			ELSE
				pb_eliminar.Enabled	= True
				pb_grabar.Enabled		= True
				pb_imprimir.Enabled		= True
				pb_ins_det.Enabled	= True


				IF ll_fila_d > 0 THEN
					istr_mant.argumento[4] = String(dw_2.Object.sere_codigo[1])
					istr_mant.argumento[5] = String(dw_2.Object.serd_codigo[1])
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

on w_maed_movtoserviadienca.create
int iCurrent
call super::create
this.dw_3=create dw_3
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_3
end on

on w_maed_movtoserviadienca.destroy
call super::destroy
destroy(this.dw_3)
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

dw_2.GetChild("sere_codigo", idwc_servicio)
idwc_servicio.SetTransObject(sqlca)
idwc_servicio.Retrieve(Integer(gi_CodPlanta))

istr_mant.argumento[1]		=	String(gi_CodPlanta)
istr_mant.argumento[2]		=	String(gi_CodExport)
dw_2.Object.mose_fecmov[1] = Date(Today())
dw_2.Object.mose_hormov[1] = Time(Now())
dw_2.Object.mose_digita[1] = gstr_us.Nombre



end event

event ue_seleccion;call super::ue_seleccion;Long		ll_null

SetNull(ll_null)

istr_busq.argum[1]	=	String(dw_2.Object.plde_codigo[1])
istr_busq.argum[2]	=	String(dw_2.Object.clie_codigo[1])
istr_busq.argum[3]	= ''

OpenWithParm(w_busc_movtoservienca, istr_busq)

istr_busq	=	Message.PowerObjectParm

IF istr_busq.argum[3] <> "" THEN
	istr_mant.argumento[3]  = istr_busq.argum[3]
	This.TriggerEvent("ue_recuperadatos")
ELSE
	pb_buscar.SetFocus()
END IF
end event

event ue_modifica_detalle;IF dw_1.RowCount() > 0 THEN
	istr_mant.argumento[1] = String(dw_2.Object.plde_codigo[1])
	istr_mant.argumento[2] = String(dw_2.Object.clie_codigo[1])
	istr_mant.agrega			= False
	istr_mant.borra			= False
	istr_mant.Argumento[6]	=	String(dw_1.Object.paen_numero[il_fila])

	OpenWithParm(iw_mantencion, istr_mant)
	
END IF
end event

event ue_antesguardar;Long	ll_numero, ll_fila

istr_mant.argumento[1] = String(dw_2.Object.plde_codigo[1])
istr_mant.argumento[2] = String(dw_2.Object.clie_codigo[1])

IF isnull(dw_2.Object.mose_numero[1]) OR dw_2.Object.mose_numero[1] = 0 THEN
	ll_numero = existefolio(Integer(istr_mant.argumento[2]),Integer(istr_mant.argumento[1]))
	dw_2.Object.mose_numero[1] = ll_numero
	
	IF ll_numero > 0 THEN
		FOR ll_fila = 1 TO dw_1.RowCount()
			dw_1.Object.mose_numero[ll_fila] = ll_numero
			istr_mant.argumento[3] = String(ll_numero)
		NEXT	
	END IF
	
	IF ll_numero > 0 THEN
		FOR ll_fila = 1 TO dw_1.RowCount()
			dw_3.Object.mose_numero[ll_fila] = ll_numero
		NEXT	
	END IF
ELSE	
	
	FOR ll_fila = 1 TO dw_3.RowCount()
		dw_3.Object.mose_numero[ll_fila] = dw_2.Object.mose_numero[1]
		istr_mant.argumento[3] = String(dw_2.Object.mose_numero[1])
	NEXT	

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

event resize;Integer	maximo, li_posic_x, li_posic_y, li_visible = 0, &
			li_Ancho = 235, li_Alto = 195, li_Siguiente = 195

IF dw_2.width > il_AnchoDw_1 THEN
	maximo		=	dw_2.width
ELSE
	dw_1.width	=	This.WorkSpaceWidth() - 400
	maximo		=	dw_1.width
END IF

dw_2.x					=	37 + Round((maximo - dw_2.width) / 2, 0)
dw_2.y					=	37

dw_1.x					=	37 + Round((maximo - dw_1.width) / 2, 0)
dw_1.y					=	64 + dw_2.Height
dw_1.height				=	This.WorkSpaceHeight() - dw_1.y - 41

li_posic_x				=	This.WorkSpaceWidth() - 270
li_posic_y				=	30 // gb_1.y + 68

IF pb_buscar.Visible THEN
	pb_buscar.x				=	li_posic_x
	pb_buscar.y				=	li_posic_y
	pb_buscar.width		=	li_Ancho
	pb_buscar.height		=	li_Alto
	li_visible ++
	li_posic_y += li_Siguiente
END IF

IF pb_nuevo.Visible THEN
	pb_nuevo.x				=	li_posic_x
	pb_nuevo.y				=	li_posic_y
	pb_nuevo.width			=	li_Ancho
	pb_nuevo.height		=	li_Alto
	li_visible ++
	li_posic_y += li_Siguiente
END IF

IF	pb_eliminar.Visible THEN
	pb_eliminar.x			=	li_posic_x
	pb_eliminar.y			=	li_posic_y
	pb_eliminar.width		=	li_Ancho
	pb_eliminar.height	=	li_Alto
	li_visible ++
	li_posic_y += li_Siguiente
END IF

IF pb_grabar.Visible THEN
	pb_grabar.x				=	li_posic_x
	pb_grabar.y				=	li_posic_y
	pb_grabar.width		=	li_Ancho
	pb_grabar.height		=	li_Alto
	li_visible ++
	li_posic_y += li_Siguiente
END IF

IF pb_imprimir.Visible THEN
	pb_imprimir.x			=	li_posic_x
	pb_imprimir.y			=	li_posic_y
	pb_imprimir.width		=	li_Ancho
	pb_imprimir.height	=	li_Alto
	li_visible ++
	li_posic_y += li_Siguiente
END IF

IF pb_salir.Visible THEN
	pb_salir.x				=	li_posic_x
	pb_salir.y				=	li_posic_y
	pb_salir.width			=	li_Ancho
	pb_salir.height		=	li_Alto
	li_visible ++
	li_posic_y += li_Siguiente
END IF

pb_ins_det.x			= li_posic_x
pb_ins_det.y			=1300
pb_ins_det.width		= li_Ancho
pb_ins_det.height		= li_Alto

pb_eli_det.x			= li_posic_x
pb_eli_det.y			= pb_ins_det.y + 180
pb_eli_det.width		= li_Ancho
pb_eli_det.height		= li_Alto
end event

type dw_1 from w_mant_encab_deta_csd`dw_1 within w_maed_movtoserviadienca
integer x = 41
integer y = 704
integer width = 3269
integer height = 1208
integer taborder = 100
string title = "Detalle de Pallets"
string dataobject = "dw_mues_movtoserviadienca"
boolean livescroll = false
end type

event dw_1::retrieveend;If RowCount() > 0 Then	pb_ins_det.Enabled = TRUE
end event

type dw_2 from w_mant_encab_deta_csd`dw_2 within w_maed_movtoserviadienca
integer x = 32
integer y = 36
integer width = 2971
integer height = 588
string dataobject = "dw_mant_movtoserviadienca"
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
			This.SetItem(Row, ls_Columna, gi_codexport)
			RETURN 1
		END IF			
		istr_mant.argumento[2]=data
		
	CASE "mose_numero"
			
		IF existeregistro(Integer(istr_mant.argumento[2]),integer(istr_mant.argumento[1]),LOng(Data)) THEN
			istr_mant.argumento[3] = Data
			w_maed_movtoserviadienca.TriggerEvent("ue_recuperadatos")
		ELSE	
			MessageBox("Atención", "Número no Existe. Ingrese otro.")
			This.SetItem(Row, ls_Columna, ll_null)
			RETURN 1
		END IF			

	CASE "plde_codigo"
		IF existe_planta(Integer(data)) THEN
			This.SetItem(Row, ls_Columna, Integer(ll_null))
			RETURN 1
		END IF	
		
		dw_2.GetChild("sere_codigo", idwc_servicio)
		idwc_servicio.SetTransObject(sqlca)
		idwc_servicio.Retrieve(Integer(data))
		
		dw_2.GetChild("serd_codigo", idwc_detalle)
		idwc_detalle.SetTransObject(sqlca)
		idwc_detalle.Retrieve(Integer(data),1)
		
		istr_mant.argumento[1] = data
		
	CASE "sere_codigo"
		IF noexisteservicios(ls_columna, Integer(data)) THEN
			This.SetItem(Row, ls_Columna, ll_null)
			RETURN 1
		END IF	
		
		istr_mant.argumento[4] = data
		
		dw_2.GetChild("serd_codigo", idwc_detalle)
		idwc_detalle.SetTransObject(sqlca)
		idwc_detalle.Retrieve(Integer(istr_mant.argumento[1]),integer(data))
		
	CASE "serd_codigo"	
		IF noexisteservicios(ls_columna, Integer(data)) THEN
			This.SetItem(Row, ls_Columna, ll_null)
			RETURN 1
		END IF	
		istr_mant.argumento[5] = data
		
END CHOOSE

HabilitaIngreso(ls_columna)
end event

event dw_2::doubleclicked;//
end event

type pb_nuevo from w_mant_encab_deta_csd`pb_nuevo within w_maed_movtoserviadienca
integer x = 3451
integer y = 300
end type

event pb_nuevo::clicked;call super::clicked;IF dw_3.RowCount() > 0 THEN
	dw_3.Reset()
END IF	
end event

type pb_eliminar from w_mant_encab_deta_csd`pb_eliminar within w_maed_movtoserviadienca
integer x = 3461
integer y = 532
end type

event pb_eliminar::clicked;Parent.TriggerEvent("ue_borrar")
end event

type pb_grabar from w_mant_encab_deta_csd`pb_grabar within w_maed_movtoserviadienca
integer x = 3451
integer y = 744
end type

event pb_grabar::clicked;Parent.TriggerEvent("ue_guardar")
end event

type pb_imprimir from w_mant_encab_deta_csd`pb_imprimir within w_maed_movtoserviadienca
integer x = 3451
integer y = 976
boolean enabled = true
boolean default = true
end type

type pb_salir from w_mant_encab_deta_csd`pb_salir within w_maed_movtoserviadienca
integer x = 3451
integer y = 1204
end type

type pb_ins_det from w_mant_encab_deta_csd`pb_ins_det within w_maed_movtoserviadienca
integer x = 3451
integer y = 1500
end type

event pb_ins_det::clicked;Parent.TriggerEvent("ue_nuevo_detalle")
end event

type pb_eli_det from w_mant_encab_deta_csd`pb_eli_det within w_maed_movtoserviadienca
integer x = 3451
integer y = 1708
end type

event pb_eli_det::clicked;Parent.TriggerEvent("ue_borra_detalle")
end event

type pb_buscar from w_mant_encab_deta_csd`pb_buscar within w_maed_movtoserviadienca
integer x = 3447
integer y = 120
end type

type dw_3 from datawindow within w_maed_movtoserviadienca
integer x = 2921
integer y = 128
integer width = 686
integer height = 400
integer taborder = 30
boolean bringtotop = true
boolean titlebar = true
string title = "none"
string dataobject = "dw_mues_movtoservicioscaracteristicas"
boolean hscrollbar = true
boolean vscrollbar = true
boolean resizable = true
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

