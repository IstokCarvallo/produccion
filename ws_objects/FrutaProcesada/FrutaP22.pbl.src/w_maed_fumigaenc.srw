$PBExportHeader$w_maed_fumigaenc.srw
forward
global type w_maed_fumigaenc from w_mant_encab_deta_csd
end type
type pb_caracteristicas from picturebutton within w_maed_fumigaenc
end type
type dw_3 from datawindow within w_maed_fumigaenc
end type
end forward

global type w_maed_fumigaenc from w_mant_encab_deta_csd
integer width = 3593
integer height = 2064
string title = "MANTENCION DE CONDICIÓN"
string menuname = ""
event ue_imprimir ( )
pb_caracteristicas pb_caracteristicas
dw_3 dw_3
end type
global w_maed_fumigaenc w_maed_fumigaenc

type variables
w_mant_deta_fumigadet iw_mantencion

DataWindowChild	dw_condicion,dw_planta, dw_plades, dw_puerto,dw_etiquetas,dw_plantas,dw_clientes, &
						dw_ptodes, dw_camaras, dw_mercado, dw_destinos
end variables

forward prototypes
public function boolean noexistecliente (integer ai_codigo)
public subroutine habilitaingreso (string columna)
public subroutine habilitaencab (boolean habilita)
public subroutine cuentatarjas ()
public function boolean existefolio (string as_columna, string as_valor)
public function boolean noexistemercado (integer ai_codigo)
protected function boolean wf_actualiza_db (boolean borrando)
public function long buscafumigacion (integer planta, integer cliente, integer condicion)
end prototypes

event ue_imprimir;SetPointer(HourGlass!)

Long		fila
str_info	lstr_info

lstr_info.titulo	= "FUMIGACION DE PALLETS"
lstr_info.copias	= 1

OpenWithParm(vinf,lstr_info)

vinf.dw_1.DataObject = "dw_info_fumigado_enc"

vinf.dw_1.SetTransObject(sqlca)

fila = vinf.dw_1.Retrieve(Integer(istr_mant.argumento[1]), &
									Long(istr_mant.argumento[2]), &
									Integer(istr_mant.argumento[3]),&
									Integer(istr_mant.argumento[9]),0)

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
Time	lt_hora
dw_2.AcceptText()

li_tarjas = dw_2.Object.fumi_cantar[1]

IF IsNull(li_tarjas) THEN li_tarjas = 0

IF dw_2.RowCount() > 0 THEN
	IF IsNull(dw_2.Object.plde_codigo[1]) OR dw_2.Object.plde_codigo[1] = 0 OR &
		IsNull(dw_2.Object.cond_codigo[1]) OR dw_2.Object.cond_codigo[1] = 0 OR &
		IsNull(dw_2.Object.merc_codigo[1]) OR dw_2.Object.merc_codigo[1] = 0 OR &
		IsNull(dw_2.Object.dest_codigo[1]) OR dw_2.Object.dest_codigo[1] = 0 OR &		
		IsNull(dw_2.Object.fumi_fecfum[1]) OR dw_2.Object.fumi_fecfum[1] = ld_fecha OR &
		li_tarjas = 0 OR &
		IsNull(dw_2.Object.fumi_horain[1]) OR dw_2.Object.fumi_horain[1] = lt_hora  THEN
		lb_estado = False
	END IF
END IF

pb_ins_det.Enabled = lb_estado
pb_caracteristicas.Enabled = lb_estado
end subroutine

public subroutine habilitaencab (boolean habilita);IF Habilita THEN
	dw_2.Object.clie_codigo.Protect	=	0
	dw_2.Object.plde_codigo.Protect	=	0
	dw_2.Object.fumi_numero.Protect	=	0
	dw_2.Object.cond_codigo.Protect	=	0

	dw_2.Object.fumi_numero.Color	=	0
	dw_2.Object.clie_codigo.Color 		=	0	
	dw_2.Object.plde_codigo.Color 	=	0
	dw_2.Object.cond_codigo.Color 	=	0
	
	dw_2.Object.fumi_numero.BackGround.Color	=	Rgb(255,255,255)
	dw_2.Object.clie_codigo.BackGround.Color 		=	Rgb(255,255,255)
	dw_2.Object.plde_codigo.BackGround.Color 	=	Rgb(255,255,255)
	dw_2.Object.cond_codigo.BackGround.Color 	=	Rgb(255,255,255)
	
	dw_2.SetColumn("fumi_numero")
	dw_2.SetFocus()
ELSE
	dw_2.Object.clie_codigo.Protect	=	1
	dw_2.Object.plde_codigo.Protect	=	1
	dw_2.Object.fumi_numero.Protect	=	1
	dw_2.Object.cond_codigo.Protect	=	1

	dw_2.Object.fumi_numero.Color	=	Rgb(255,255,255)
	dw_2.Object.clie_codigo.Color 		=	Rgb(255,255,255)
	dw_2.Object.plde_codigo.Color 	=	Rgb(255,255,255)
	dw_2.Object.cond_codigo.Color 	=	Rgb(255,255,255)
	
	dw_2.Object.fumi_numero.BackGround.Color	=	553648127
	dw_2.Object.clie_codigo.BackGround.Color 		=	553648127
	dw_2.Object.plde_codigo.BackGround.Color 	=	553648127
	dw_2.Object.cond_codigo.BackGround.Color 	=	553648127
END IF

IF dw_2.Object.fumi_estado[1] = 1 THEN
	dw_1.Enabled = False
	dw_2.Enabled = False
ELSE	
	dw_1.Enabled = True
	dw_2.Enabled = True
END IF	


end subroutine

public subroutine cuentatarjas ();Long I,ll_tra=0,ll_def=0

FOR I=1 TO dw_1.Rowcount()
		ll_def ++
NEXT
istr_mant.argumento[10]	= String(dw_2.Object.fumi_cantar[1] )
		
RETURN
end subroutine

public function boolean existefolio (string as_columna, string as_valor);Integer	li_planta, li_existe, li_cliente, li_condicion
Long		ll_nfolio

li_cliente		=	dw_2.Object.clie_codigo[1]
li_planta		=	dw_2.Object.plde_codigo[1]
ll_nfolio 		=	dw_2.Object.fumi_numero[1]
li_condicion	=  dw_2.Object.cond_codigo[1]

CHOOSE CASE as_columna
	CASE "clie_codigo"
		li_cliente	=	Integer(as_valor)
		
	CASE "plde_codigo"
		li_planta	=	Integer(as_valor)

	CASE "fumi_numero"
		ll_nfolio 	=	Long(as_valor)
		
	CASE "cond_codigo"
		li_condicion 	=	Long(as_valor)	
END CHOOSE

SELECT	Count(*) 
	INTO	 :li_existe
	FROM	dbo.FUMIGAENC
	WHERE	plde_codigo	=	:li_planta
	AND	clie_codigo 		=  :li_cliente
	AND	fumi_numero	=	:ll_nfolio
	AND   cond_codigo 	=  :li_condicion
	AND	isnull(fumi_estado,1) in	(1,2,7);
				
IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla Fumigaenc")
	RETURN True
ELSE
	IF li_existe > 0 THEN
		istr_mant.argumento[1]	=	String(li_planta)
		istr_mant.argumento[2]	=	String(ll_nfolio)
		istr_mant.argumento[9]	=	String(li_condicion)
		
		This.TriggerEvent("ue_recuperadatos")
		istr_mant.argumento[3]	=	String(dw_2.Object.clie_codigo[1])
		istr_mant.argumento[4]	=	String(dw_2.Object.fumi_cantar[1])
		istr_mant.argumento[8]	=	String(dw_2.Object.fumi_cantcaj[1])
		RETURN False
	ELSE
		istr_mant.argumento[1]	=	String(li_planta)
		istr_mant.argumento[2]	=	String(ll_nfolio)
		istr_mant.argumento[3]	=	String(li_cliente)
		istr_mant.argumento[9]	=	String(li_condicion)
		
		SELECT	Count(*) 
		INTO	 :li_existe
		FROM	dbo.FUMIGAENC
		WHERE	plde_codigo	=	:li_planta
		AND	clie_codigo =  :li_cliente
		AND	fumi_numero	=	:ll_nfolio
		AND   cond_codigo =  :li_condicion
		AND	isnull(fumi_estado,0) =	2;
		
		IF li_existe > 0 THEN
			MessageBox("Atención","Número de Condición se Encuentra Rechazado.", Exclamation!, Ok!)
			RETURN True
		END IF		
	END IF
	RETURN False
END IF

end function

public function boolean noexistemercado (integer ai_codigo);String	ls_nombre

  SELECT merc_nombre  
    INTO :ls_nombre
    FROM dbo.mercado  
   WHERE merc_codigo = :ai_codigo ;
	
	
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura Tabla Mercado")
	RETURN True
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Error", "Mercado no Existe. Ingrese otro.")
	RETURN True
ELSE
	RETURN False
END IF




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
		dw_1.Object.fumi_numero[li_fila]	=	Long(dw_2.Object.fumi_numero[1])
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
	For li_fila = 1 to dw_1.RowCount()
		dw_1.Object.fumi_numero[li_fila]	=	Long(dw_2.Object.fumi_numero[1])
		dw_1.Object.cond_codigo[li_fila]	=	dw_2.Object.cond_codigo[1]
	Next
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

Numero = Long(istr_mant.argumento[2])
li_planta = Integer(istr_mant.argumento[1])

li_movto = 7

/*actualiza numero actual en correlativos */
update dbo.CORRELMOVIMIENTOS set
COMO_ACTUAl = :numero
where plde_codigo = :li_planta
And	como_tipomv = :li_movto;

sqlca.AutoCommit	=	lb_AutoCommit

RETURN lb_Retorno
end function

public function long buscafumigacion (integer planta, integer cliente, integer condicion);Long		ll_numero

Select Max(fumi_numero) + 1
Into  :ll_numero
From dbo.fumigaenc
Where plde_codigo = :planta
And	clie_codigo = :cliente	
And	cond_codigo = :condicion;

IF isnull(ll_numero) OR ll_numero = 0 THEN ll_numero = 1

RETURN ll_numero

end function

event open;/*
  istr_mant.argumento[1]	= li_planta
  istr_mant.argumento[2]	= nfolio
  istr_mant.argumento[3]	= Cliente
  istr_mant.argumento[9]	= Condicion
*/
String ls_filtro

ls_filtro = '<> 0'

IF gi_CodExport = 300 THEN dw_2.Object.plde_codigo.dddw.Name	=	"dw_mues_plantadesp"

dw_2.GetChild("plde_codigo", dw_planta)
dw_planta.SetTransObject(sqlca)
dw_planta.Retrieve(1)

dw_2.GetChild("clie_codigo", dw_clientes)
dw_clientes.SetTransObject(sqlca)
dw_clientes.Retrieve(gi_CodExport)

dw_2.GetChild("cama_codigo", dw_camaras)
dw_camaras.SetTransObject(sqlca)
dw_camaras.Retrieve(gi_CodPlanta,2)
dw_camaras.insertRow(0)

dw_2.GetChild("merc_codigo", dw_mercado)
dw_mercado.SetTransObject(sqlca)
dw_mercado.Retrieve(-1)
dw_mercado.insertRow(0)

dw_2.GetChild("dest_codigo", dw_destinos)
dw_destinos.SetTransObject(sqlca)
dw_destinos.Retrieve(-1)
dw_destinos.insertRow(0)

dw_2.GetChild("cond_codigo", dw_condicion)
dw_condicion.SetTransObject(sqlca)
dw_condicion.Retrieve(-1)
dw_condicion.insertRow(0)
dw_condicion.SetFilter("cond_codigo <> 0 " )
dw_condicion.Filter()

call super::open

buscar	= "Pallet Nuevo:Npaen_numero,Descripción:Svari_nombre"
ordenar	= "Pallet Nuevo:paen_numero,Descripción:vari_nombre"

istr_mant.argumento[1]	=	String(gi_CodPlanta)
istr_mant.argumento[3]	=	String(gi_CodExport)
istr_mant.argumento[9]	=	'1'

//istr_mant.argumento[5]	=	'dw_mues_repalletdeta_salida_cambiofolio'

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
		pb_caracteristicas.Enabled	=	False
	END IF
END IF

istr_mant.borra	 = False
end event

event ue_nuevo_detalle;call super::ue_nuevo_detalle;istr_mant.borra	= False
istr_mant.agrega	= True

IF dw_1.RowCount() >= (dw_2.Object.fumi_cantar[1]) THEN
	MessageBox("Atención", "No puede ingresar más Tarjas.")
ELSE

	istr_mant.Argumento[7]	=	"0"
	istr_mant.Argumento[16]	=	"0"
	istr_mant.Argumento[21]	=	String(dw_2.Object.plde_codigo[1])
	
	CuentaTarjas()		
	
	OpenWithParm(iw_mantencion, istr_mant)
	
	IF dw_1.RowCount() > 0 THEN HabilitaEncab(False)
	
	IF dw_1.RowCount() > 0 and pb_eliminar.Enabled = FALSE THEN
		pb_eliminar.Enabled	= TRUE
		pb_grabar.Enabled		= TRUE
	END IF
		
	dw_1.SetRow(il_fila)
	dw_1.SelectRow(il_fila,True)
END IF

end event

event ue_recuperadatos;call super::ue_recuperadatos;Long	ll_fila_d, ll_fila_e, respuesta
DO
	dw_2.SetRedraw(False)
	dw_2.Reset()
	ll_fila_e	= dw_2.Retrieve(Integer(istr_mant.argumento[1]), Long(istr_mant.argumento[2]),Integer(istr_mant.argumento[3]),Integer(istr_mant.argumento[9]))

	IF ll_fila_e = -1 THEN
		respuesta = MessageBox(	"Error en Base de Datos", "No es posible conectar la Base de Datos.", &
										Information!, RetryCancel!)
									
	ELSE
		DO
			dw_2.GetChild("cama_codigo", dw_camaras)
			dw_camaras.SetTransObject(sqlca)
			dw_camaras.Retrieve(Integer(istr_mant.argumento[1]),2)
						
			ll_fila_d	= dw_1.Retrieve(Integer(istr_mant.argumento[1]), Long(istr_mant.argumento[2]), Integer(istr_mant.argumento[3]),Integer(istr_mant.argumento[9]))

			IF ll_fila_d = -1 THEN
				respuesta = MessageBox(	"Error en Base de Datos", "No es posible conectar la Base de Datos.", &
												Information!, RetryCancel!)
			ELSE
				pb_eliminar.Enabled			= True
				pb_grabar.Enabled			= True
				pb_imprimir.Enabled			= True
				pb_ins_det.Enabled			= True
				pb_caracteristicas.Enabled	= True

				IF ll_fila_d > 0 THEN
				   pb_eli_det.Enabled	= True
					dw_1.SetRow(1)
					dw_1.SelectRow(1,True)
					dw_1.SetFocus()
					HabilitaEncab(False)
					IF ll_fila_d = dw_2.Object.fumi_cantar[1] THEN pb_ins_det.Enabled = False
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

on w_maed_fumigaenc.create
int iCurrent
call super::create
this.pb_caracteristicas=create pb_caracteristicas
this.dw_3=create dw_3
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.pb_caracteristicas
this.Control[iCurrent+2]=this.dw_3
end on

on w_maed_fumigaenc.destroy
call super::destroy
destroy(this.pb_caracteristicas)
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

pb_caracteristicas.Enabled	=	False

dw_2.GetChild("cama_codigo", dw_camaras)
dw_camaras.SetTransObject(sqlca)
dw_camaras.Retrieve(gi_CodPlanta,2)
dw_camaras.insertRow(0)

dw_2.SetRedraw(False)
dw_2.Reset()
dw_2.InsertRow(0)
dw_2.SetRedraw(True)

dw_2.SetFocus()

HabilitaEncab(True)

dw_2.SetItem(1, "clie_codigo", gi_CodExport)
dw_2.SetItem(1, "plde_codigo", gi_CodPlanta)
dw_2.SetItem(1, "cond_codigo", 1)
dw_2.SetItem(1, "fumi_estado", 7)

dw_2.Object.fumi_numero[1] = buscafumigacion(gi_CodPlanta,gi_CodExport,1)

istr_mant.argumento[1]	=	String(gi_CodPlanta)
istr_mant.argumento[3]	=	String(gi_CodExport)
istr_mant.argumento[9]	=	'1'


end event

event ue_seleccion;call super::ue_seleccion;Long		ll_null

SetNull(ll_null)

istr_busq.argum[1]	=	String(dw_2.Object.clie_codigo[1])
istr_busq.argum[2]	=	String(dw_2.Object.plde_codigo[1])
istr_busq.argum[3]   =  String(dw_2.Object.cond_codigo[1])

OpenWithParm(w_busc_fumigaenc, istr_busq)

istr_busq	=	Message.PowerObjectParm

IF istr_busq.argum[5] <> "" THEN
	istr_mant.argumento[1]	= istr_busq.argum[2]
	istr_mant.argumento[2]	= istr_busq.argum[5]
	istr_mant.argumento[3]  = istr_busq.argum[1]
	IF	istr_mant.argumento[2]	<>	""	THEN
		dw_2.SetItem(1, "fumi_numero", Long(istr_mant.argumento[2]))
		IF ExisteFolio('fumi_numero', String(dw_2.Object.fumi_numero[1])) THEN
			dw_2.SetFocus()
			dw_2.SetColumn("fumi_numero")
			dw_2.SetItem(1, "fumi_numero", ll_null)			
			RETURN
		END IF
	ELSE
		This.TriggerEvent("ue_recuperadatos")
	END IF
ELSE
	pb_buscar.SetFocus()
END IF
end event

event ue_modifica_detalle;IF dw_1.RowCount() > 0 THEN
	istr_mant.agrega			= False
	istr_mant.borra			= False
	istr_mant.Argumento[6]	=	String(dw_1.Object.paen_numero[il_fila])

	//OpenWithParm(w_maed_palletencab_consulta, istr_mant)
	OpenWithParm(w_mant_deta_fumigadet, istr_mant)
	
END IF
end event

event ue_antesguardar;IF isnull(dw_2.Object.fumi_numero[1]) OR dw_2.Object.fumi_numero[1] = 0 THEN
	MessageBox("Atención","Falta Correlativo del Tipo de la Condición Seleccionada")
	Message.DoubleParm = -1
	Return 
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

event resize;call super::resize;pb_caracteristicas.x			= pb_salir.x
pb_caracteristicas.y			= pb_salir.y + pb_salir.Height + 10
end event

type dw_1 from w_mant_encab_deta_csd`dw_1 within w_maed_fumigaenc
integer x = 55
integer y = 864
integer width = 2880
integer height = 992
integer taborder = 100
string title = "Detalle de Pallets"
string dataobject = "dw_mues_fumigadet"
boolean livescroll = false
end type

event dw_1::retrieveend;If RowCount() > 0 Then	pb_ins_det.Enabled = TRUE
end event

type dw_2 from w_mant_encab_deta_csd`dw_2 within w_maed_fumigaenc
integer x = 50
integer y = 68
integer width = 2885
integer height = 752
string dataobject = "dw_mant_fumigaenc"
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
		istr_mant.argumento[3]=data
		
		dw_2.Object.fumi_numero[1] = buscafumigacion(dw_2.Object.plde_codigo[1],Integer(data),dw_2.Object.cond_codigo[1])
		
		IF NoExisteCliente(Integer(data)) THEN
			This.SetItem(Row, ls_Columna, gi_codexport)
			RETURN 1
		ELSEIF ExisteFolio(ls_columna, data) THEN
				This.SetItem(Row, ls_Columna, ll_null)
				RETURN 1
			ELSE
				
            dw_2.GetChild("plde_codigo", dw_planta)
			   dw_planta.SetTransObject(sqlca)
			   dw_planta.Retrieve(1)
						
		END IF			

	CASE "plde_codigo"
		IF ExisteFolio(ls_columna, data) THEN
			This.SetItem(Row, ls_Columna, ll_null)
			RETURN 1
		END IF			
		li_Planta	=	Integer(data)
		//dw_camaras.Retrieve(li_Planta,2)
		dw_2.GetChild("cama_codigo", dw_camaras)
		dw_camaras.SetTransObject(sqlca)
		dw_camaras.Retrieve(li_planta,2)
		dw_camaras.insertRow(0)
		dw_2.Object.fumi_numero[1] = buscafumigacion(li_planta,dw_2.object.clie_codigo[1],Integer(istr_mant.argumento[9]))
		
	CASE "cond_codigo"
		If data = '2' Then 
			This.Object.esta_codigo[Row] = 9
		Else
			This.Object.esta_codigo[Row] = 8
		End If

		IF data ='7' THEN
			MessageBox("Atención","Número de Código Reservado, Ingrese Otro",Information!, Ok!)
			This.SetItem(row, ls_columna, Integer(ll_null))
			RETURN 1
		END IF	
			
		istr_mant.argumento[9] = data
		
		dw_2.Object.fumi_numero[1] = buscafumigacion(dw_2.Object.plde_codigo[1],dw_2.object.clie_codigo[1],Integer(data))

		
	CASE "fumi_numero"
		IF ExisteFolio(ls_columna, data) THEN
			This.SetItem(Row, ls_Columna, ll_null)
			RETURN 1
		END IF			

	CASE "fumi_cantar"
			istr_mant.argumento[4] = data

	CASE "fumi_cantcaj"
			istr_mant.argumento[8] = String(Long(data))
	
	CASE "fumi_fecfum"
		IF Not f_validafechatempo(date(data)) THEN
			This.SetItem(Row, ls_Columna, ld_nula)
			RETURN 1
		END IF	
		
	CASE "merc_codigo"
		IF NoExisteMercado(Integer(data)) THEN
			This.SetItem(Row, ls_Columna, Integer(ll_null))
			RETURN 1
		ELSE
			dw_2.GetChild("dest_codigo", dw_destinos)
			dw_destinos.SetTransObject(sqlca)
			dw_destinos.Retrieve(Integer(data))
		END IF		

END CHOOSE

HabilitaIngreso(ls_columna)

end event

event dw_2::doubleclicked;//
end event

type pb_nuevo from w_mant_encab_deta_csd`pb_nuevo within w_maed_fumigaenc
integer x = 3145
long backcolor = 553648127
end type

type pb_eliminar from w_mant_encab_deta_csd`pb_eliminar within w_maed_fumigaenc
integer x = 3141
integer y = 608
long backcolor = 553648127
end type

event pb_eliminar::clicked;IF dw_2.Object.fumi_estado[1] = 2 THEN
	MessageBox("Atención","Estado debe estar pendiente para Borrar.", Exclamation!, Ok!)
	RETURN
END IF	

IF dw_2.Object.fumi_estado[1] <> 7 THEN
	MessageBox("Atención","Estado debe estar pendiente para Borrar.", Exclamation!, Ok!)
	RETURN
END IF	

Parent.TriggerEvent("ue_borrar")
end event

type pb_grabar from w_mant_encab_deta_csd`pb_grabar within w_maed_fumigaenc
integer x = 3150
integer y = 728
long backcolor = 553648127
end type

event pb_grabar::clicked;IF dw_2.Object.fumi_estado[1] = 2 THEN
	MessageBox("Atención","Estado debe estar pendiente para Grabar.", Exclamation!, Ok!)
	RETURN
END IF

IF dw_2.Object.fumi_estado[1] <> 7 THEN
	MessageBox("Atención","Estado debe estar pendiente para Borrar.", Exclamation!, Ok!)
	RETURN
END IF	

Parent.TriggerEvent("ue_guardar")
end event

type pb_imprimir from w_mant_encab_deta_csd`pb_imprimir within w_maed_fumigaenc
boolean visible = false
integer x = 3150
integer y = 960
boolean enabled = true
boolean default = true
long backcolor = 553648127
end type

type pb_salir from w_mant_encab_deta_csd`pb_salir within w_maed_fumigaenc
integer x = 3150
integer y = 1188
long backcolor = 553648127
end type

type pb_ins_det from w_mant_encab_deta_csd`pb_ins_det within w_maed_fumigaenc
integer x = 3150
integer y = 1516
long backcolor = 553648127
end type

event pb_ins_det::clicked;IF dw_2.Object.fumi_estado[1] = 2 THEN
	MessageBox("Atención","Estado debe estar pendiente para Modificar.", Exclamation!, Ok!)
	RETURN
END IF	

IF dw_2.Object.fumi_estado[1] <> 7 THEN
	MessageBox("Atención","Estado debe estar pendiente para Modificar.", Exclamation!, Ok!)
	RETURN
END IF	

Parent.TriggerEvent("ue_nuevo_detalle")
end event

type pb_eli_det from w_mant_encab_deta_csd`pb_eli_det within w_maed_fumigaenc
integer x = 3150
integer y = 1692
long backcolor = 553648127
end type

event pb_eli_det::clicked;IF dw_2.Object.fumi_estado[1] = 2 THEN
	MessageBox("Atención","Estado debe estar pendiente para Modificar.", Exclamation!, Ok!)
	RETURN
END IF	

IF dw_2.Object.fumi_estado[1] <> 7 THEN
	MessageBox("Atención","Estado debe estar pendiente para Modificar.", Exclamation!, Ok!)
	RETURN
END IF	

Parent.TriggerEvent("ue_borra_detalle")
end event

type pb_buscar from w_mant_encab_deta_csd`pb_buscar within w_maed_fumigaenc
integer x = 3145
integer y = 104
long backcolor = 553648127
end type

type pb_caracteristicas from picturebutton within w_maed_fumigaenc
integer x = 3150
integer y = 1348
integer width = 302
integer height = 244
integer taborder = 100
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string picturename = "\Desarrollo 17\Imagenes\Botones\apuntes.png"
string disabledname = "\Desarrollo 17\Imagenes\Botones\apuntes-bn.png"
alignment htextalign = left!
long backcolor = 553648127
end type

event clicked;Integer	li_inspec, li_continua = 0, li_especie, li_variedad, li_categoria, li_cliente, li_planta
Long		ll_fila, ll_fila1, ll_fila2, ll_new, ll_pallet, ll_numinpe, li_tipo
String	ls_calibre, ls_embalaje

istr_mant.argumento[17] = '1'

OpenWithParm(w_caracteristicas_condicion, istr_mant)

istr_mant	       = Message.PowerObjectParm

li_especie 	 = Integer(istr_mant.argumento[11])
li_variedad	 = Integer(istr_mant.argumento[12])
li_categoria = Integer(istr_mant.argumento[13])
ls_calibre	 = String(istr_mant.argumento[14])
ls_embalaje  = String(istr_mant.argumento[15])
li_cliente	 = Integer(istr_mant.argumento[3])
li_planta	 = Integer(istr_mant.argumento[1])
li_tipo		 = Integer(istr_mant.argumento[16]) //tipo por inspeccion o por caracteristicas

dw_3.SetTransObject(sqlca)

IF li_tipo = 1 THEN
	IF istr_mant.argumento[17]	= '-1' THEN
		Return 1
	END IF 	
	
	ll_numinpe	 = Long(istr_mant.argumento[18]) //Numero de inspeccion
	//MessageBox( 'Message', 'Result' + string(ll_numinpe))
	dw_3.DataObject = "dw_traspasoinfo_condicion_inspecion"
	dw_3.SetTransObject(sqlca)
	ll_fila = dw_3.Retrieve(li_cliente, li_planta, ll_numinpe)
	
ELSE	
	IF istr_mant.argumento[17]	= '-1' THEN
		Return 1
	END IF 	
	
	dw_3.DataObject = "dw_traspasoinfo_condicion"
	dw_3.SetTransObject(sqlca)
	ll_fila = dw_3.Retrieve(li_especie, li_variedad, li_categoria, ls_calibre, ls_embalaje, li_cliente, li_planta)
END IF	
	
IF ll_fila > 0 THEN
	FOR ll_fila1 = 1 TO dw_3.Rowcount() 
		li_continua = 0	
		li_inspec = dw_3.Object.paen_inspec[ll_fila1]
		
		/*	Verifica Condición de Inspección	*/
		IF li_inspec <> 0 AND li_tipo = 2 THEN
			IF MessageBox("Advertencia", "Se quiere Fumigar un Pallet Inspeccionado.~r~r" + &
							"Desea continuar ?", Question!, YesNo!, 2) = 2 THEN 
				li_continua = 1
			ELSE
				li_continua = 0
			END IF	
			
			IF li_continua = 0 THEN
								
				ll_pallet = dw_3.Object.paen_numero[ll_fila1]
								
				ll_fila2	=	dw_1.Find("clie_codigo = " + istr_mant.Argumento[3] + &
										" AND plde_codigo = " + istr_mant.Argumento[1] + &
										" AND paen_numero = " + String(ll_pallet) , 1, dw_1.RowCount())
					
				IF ll_fila2 = 0 THEN			
					ll_new = dw_1.InsertRow(0)
					dw_1.Object.clie_codigo[ll_new] = Integer(istr_mant.argumento[3])
					dw_1.Object.plde_codigo[ll_new] = Integer(istr_mant.argumento[1])   
					dw_1.Object.paen_numero[ll_new] = dw_3.Object.paen_numero[ll_fila1]   
					dw_1.Object.vari_nombre[ll_new] = dw_3.Object.vari_nombre[ll_fila1]
					dw_1.Object.emba_codigo[ll_new] = dw_3.Object.emba_codigo[ll_fila1]
					dw_1.Object.palletencab_paen_ccajas[ll_new] = dw_3.Object.paen_ccajas[ll_fila1]
					dw_1.Object.cond_codigo[ll_new] = dw_2.Object.cond_codigo[1]
				END IF
			END IF	
		ELSE
			
			ll_pallet = dw_3.Object.paen_numero[ll_fila1]
								
			ll_fila2	=	dw_1.Find("clie_codigo = " + istr_mant.Argumento[3] + &
									" AND plde_codigo = " + istr_mant.Argumento[1] + &
									" AND paen_numero = " + String(ll_pallet) , 1, dw_1.RowCount())
			
			IF ll_fila2 = 0 THEN
				ll_new = dw_1.InsertRow(0)
							
				dw_1.Object.clie_codigo[ll_new] = Integer(istr_mant.argumento[3])
				dw_1.Object.plde_codigo[ll_new] = Integer(istr_mant.argumento[1])   
				dw_1.Object.paen_numero[ll_new] = dw_3.Object.paen_numero[ll_fila1]   
				dw_1.Object.vari_nombre[ll_new] = dw_3.Object.vari_nombre[ll_fila1]
				dw_1.Object.emba_codigo[ll_new] = dw_3.Object.emba_codigo[ll_fila1]
				dw_1.Object.palletencab_paen_ccajas[ll_new] = dw_3.Object.paen_ccajas[ll_fila1]
				dw_1.Object.cond_codigo[ll_new] = dw_2.Object.cond_codigo[1]
			END IF				
		END IF
		pb_grabar.Enabled = True
	NEXT	
ELSE
	IF istr_mant.argumento[17] = '1' THEN
		MessageBox("Atención", "Caracteristicas Ingresadas NO Retornan Registros o ya se encuentran Fumigados.", &
			Exclamation!, OK!)
		Return 		
	END IF		
END IF


end event

type dw_3 from datawindow within w_maed_fumigaenc
boolean visible = false
integer x = 763
integer y = 1888
integer width = 686
integer height = 400
integer taborder = 100
boolean bringtotop = true
string title = "none"
string dataobject = "dw_traspasoinfo_condicion"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

