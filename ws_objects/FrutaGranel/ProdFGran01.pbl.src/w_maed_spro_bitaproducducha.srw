$PBExportHeader$w_maed_spro_bitaproducducha.srw
forward
global type w_maed_spro_bitaproducducha from w_mant_encab_deta_csd
end type
end forward

global type w_maed_spro_bitaproducducha from w_mant_encab_deta_csd
integer width = 2779
integer height = 2320
string title = "RELLENO DE DUCHA"
string menuname = ""
event ue_imprimir ( )
event ue_validaregistro ( )
end type
global w_maed_spro_bitaproducducha w_maed_spro_bitaproducducha

type variables
Integer ii_seleccion
uo_duchacontrol	iuo_duchacontrol
end variables

forward prototypes
protected function boolean wf_actualiza_db (boolean borrando)
public function boolean duplicado (string codigo)
public function boolean existeducha ()
public function boolean existeencabezado (string as_columna, string as_valor)
public subroutine habilitabotton (string columna)
public subroutine habilitaencab (boolean habilita)
public subroutine buscaducha ()
end prototypes

event ue_imprimir();SetPointer(HourGlass!)

Long		fila
str_info	lstr_info

lstr_info.titulo	= "MAESTRO DE TIPOS DE MOVIMIENTOS / DOCUMENTOS DE MOVIMIENTOS"
lstr_info.copias	= 1

OpenWithParm(vinf,lstr_info)

vinf.dw_1.DataObject = "dw_info_tipomovtofruta_doctomovtodeta"

vinf.dw_1.SetTransObject(sqlca)

fila = vinf.dw_1.Retrieve(Integer(istr_mant.argumento[1]))

IF fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
					"de Datos : ~n" + sqlca.SQLErrText, StopSign!, OK!)
ELSEIF fila = 0 THEN
	MessageBox( "No Existe información", "No Existe información para este informe.", &
					StopSign!, OK!)
	
ELSE
	F_Membrete(vinf.dw_1)
	vinf.dw_1.Modify('DataWindow.Print.Preview = Yes')
	vinf.dw_1.Modify('DataWindow.Print.Preview.Zoom = 75')
	
	vinf.Visible	= True
	vinf.Enabled	= True
END IF

SetPointer(Arrow!)
					
end event

event ue_validaregistro();Integer	li_cont
String	ls_mensaje, ls_colu[]
	

	IF Isnull(dw_1.Object.prdu_codigo[il_fila]) OR dw_1.Object.prdu_codigo[il_fila] = "" THEN
		li_cont ++
		ls_mensaje 			= ls_mensaje + "~nCódigo Producto "
		ls_colu[li_cont]	= "prdu_codigo"
	END IF

IF li_cont > 0 THEN
	MessageBox("Error de Consistencia", "Falta el ingreso de :" + ls_mensaje + ".", StopSign!, Ok!)
	dw_1.SetColumn(ls_colu[1])
	dw_1.SetFocus()
	Message.DoubleParm = -1
END IF
end event

protected function boolean wf_actualiza_db (boolean borrando);Boolean	lb_AutoCommit, lb_Retorno


IF Not dw_2.uf_check_required(0) THEN RETURN False

IF Not dw_1.uf_validate(0) THEN RETURN False

lb_AutoCommit		=	sqlca.AutoCommit
sqlca.AutoCommit	=	False

IF Borrando THEN
	IF dw_1.Update(True, False) = 1 THEN
			IF dw_2.Update(True, False) = 1 THEN
				Commit;
				
				IF sqlca.SQLCode <> 0 THEN
					F_ErrorBaseDatos(sqlca, This.Title)
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
		
		RollBack;
	END IF
ELSE
	IF dw_2.Update(True, False) = 1 THEN
			IF dw_1.Update(True, False) = 1 THEN
				Commit;
				
				IF sqlca.SQLCode <> 0 THEN
					F_ErrorBaseDatos(sqlca, This.Title)
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
		
		RollBack;
	END IF
END IF

sqlca.AutoCommit	=	lb_AutoCommit

RETURN lb_Retorno
end function

public function boolean duplicado (string codigo);Long		ll_fila
String	ls_codigo
	
ll_fila	= dw_1.Find("prdu_codigo = '"  + codigo + "'", 1, dw_1.RowCount())
	
	
IF ll_fila > 0 and ll_fila <> il_fila THEN
	MessageBox("Error","Código de Producto , ya fue Ingresado Anteriormente Para este Código de Ducha",Information!, Ok!)
	RETURN True
ELSE
	RETURN False
END IF



		
end function

public function boolean existeducha ();Boolean	lb_Retorno
Integer	li_Ducha, li_Estanque, li_Cantidad
Date		ld_Fecha, ld_FechaNula
Time		lt_Hora
String   ls_Fecha, ls_Hora

li_Ducha		= dw_2.Object.duch_codigo[1]
li_Estanque	= dw_2.Object.codu_nropos[1]
ld_Fecha		= dw_2.Object.codu_fecini[1]
lt_Hora		= dw_2.Object.codu_horini[1]

SELECT	Count(*)
	INTO	:li_Cantidad
	FROM	dba.spro_duchacontrol
	WHERE	duch_codigo	= :li_Ducha
	AND	codu_nropos	= :li_Estanque
	AND	codu_fecini	= :ld_Fecha
	AND	codu_horini	= :lt_Hora;

IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de Tabla spro_bitaproducducha")
	
	RETURN TRUE
ELSEIF sqlca.SQLCode <> 100 and li_Cantidad > 0 THEN
	lb_Retorno	=	True

ELSE
	lb_Retorno	=	False

END IF

RETURN lb_Retorno
end function

public function boolean existeencabezado (string as_columna, string as_valor);Boolean	lb_Retorno
Integer	li_Ducha, li_Estanque, li_Cantidad
Date		ld_Fecha,  ld_FechaEv,ld_FechaNula
Time		lt_Hora,	lt_HoraEv
String   ls_Fecha, ls_Hora, ls_FechaEv, ls_HoraEv

li_Ducha		= dw_2.Object.duch_codigo[1]
li_Estanque	= dw_2.Object.codu_nropos[1]
ld_Fecha	= dw_2.Object.codu_fecini[1]
lt_Hora		= dw_2.Object.codu_horini[1]
ld_FechaEv	= dw_2.Object.bpdu_feceve[1]
lt_HoraEv	= dw_2.Object.bpdu_horeve[1]

CHOOSE CASE as_Columna
	CASE "duch_codigo"
		li_Ducha		=	Integer(as_Valor)

	CASE "codu_nropos"
		li_Estanque	=	Integer(as_Valor)

	CASE "codu_fecini"
		ld_Fecha	=	Date(Mid(as_Valor, 1, 10))

	CASE "codu_horini"
		ls_Fecha	=  Mid(String(ld_fecha),1,10)
		ls_Hora	=  Mid(as_valor,12,5)
		ls_Hora  =  ls_Hora + ':00'
		lt_Hora	=	Time(ls_Hora)
      dw_2.Object.codu_horini[1]	=	lt_Hora

	CASE "bpdu_feceve"
		ld_FechaEv	=	Date(Mid(as_Valor, 1, 10))

	CASE "bpdu_horeve"
		ls_FechaEv	=  Mid(String(ld_FechaEv),1,10)
		ls_HoraEv	=  Mid(as_valor,12,5)
		ls_HoraEv  	=  ls_HoraEv + ':00'
		lt_HoraEv	=	Time(ls_HoraEv)
      dw_2.Object.bpdu_horeve[1]	=	lt_HoraEv

END CHOOSE

SELECT	Count(*)
	INTO	:li_Cantidad
	FROM	dba.spro_bitaproducducha
	WHERE	duch_codigo	= :li_Ducha
	AND	codu_nropos	= :li_Estanque
	AND	codu_fecini	= :ld_Fecha
	AND	codu_horini	= :lt_Hora
	AND	bpdu_feceve = :ld_FechaEv
	AND	bpdu_horeve = :lt_HoraEv;

IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de Tabla spro_bitaproducducha")
	
	RETURN TRUE
ELSEIF sqlca.SQLCode <> 100 and li_Cantidad > 0 THEN
	istr_mant.argumento[1]	=	String(li_Ducha)
	istr_mant.argumento[2]	=	String(li_Estanque)
	istr_mant.argumento[3]	=	String(ld_Fecha)
	istr_mant.argumento[4]	=	String(lt_Hora)
	istr_mant.argumento[6]	=	String(ld_FechaEv)
	istr_mant.argumento[7]	=	String(lt_HoraEv)

	This.TriggerEvent("ue_recuperadatos")

	lb_Retorno	=	True
ELSE
	lb_Retorno	=	False
END IF

RETURN lb_Retorno
end function

public subroutine habilitabotton (string columna);Boolean	lb_Estado = True
Date	ld_Fecha
Time	lt_Hora
	
IF IsNull(dw_2.Object.duch_codigo[il_fila]) OR dw_2.Object.duch_codigo[il_fila] = 0 THEN
	lb_Estado	=	False
END IF

IF IsNull(dw_2.Object.codu_nropos[il_fila]) OR dw_2.Object.codu_nropos[il_fila] = 0 THEN
	lb_Estado	=	False
END IF

IF IsNull(dw_2.Object.codu_fecini[il_fila]) OR dw_2.Object.codu_fecini[il_fila] = ld_Fecha THEN
	lb_Estado	=	False
END IF

IF IsNull(dw_2.Object.codu_horini[il_fila]) OR dw_2.Object.codu_horini[il_fila] = lt_Hora THEN
	lb_Estado	=	False
END IF

IF IsNull(dw_2.Object.bpdu_feceve[il_fila]) OR dw_2.Object.bpdu_feceve[il_fila] = ld_Fecha THEN
	lb_Estado	=	False
END IF

pb_ins_det.Enabled	=	lb_Estado
end subroutine

public subroutine habilitaencab (boolean habilita);IF Habilita THEN
	dw_2.Object.duch_codigo.Protect				=	0
	dw_2.Object.codu_nropos.Protect				=	0
	dw_2.Object.codu_fecini.Protect				=	0
	dw_2.Object.codu_horini.Protect				=	0
	dw_2.Object.duch_codigo.BackGround.Color	=	RGB(255,255,255)
	dw_2.Object.codu_nropos.BackGround.Color	=	RGB(255,255,255)
	dw_2.Object.codu_fecini.BackGround.Color	=	RGB(255,255,255)
	dw_2.Object.codu_horini.BackGround.Color	=	RGB(255,255,255)
ELSE
	dw_2.Object.duch_codigo.Protect				=	1
	dw_2.Object.codu_nropos.Protect				=	1
	dw_2.Object.codu_fecini.Protect				=	1
	dw_2.Object.codu_horini.Protect				=	1
	dw_2.Object.duch_codigo.BackGround.Color	=	RGB(192,192,192)
	dw_2.Object.codu_nropos.BackGround.Color	=	RGB(192,192,192)
	dw_2.Object.codu_fecini.BackGround.Color	=	RGB(192,192,192)
	dw_2.Object.codu_horini.BackGround.Color	=	RGB(192,192,192)
END IF
end subroutine

public subroutine buscaducha ();str_busqueda	lstr_busq
Date				ld_FechaSistema
Time				lt_HoraSistema

lstr_busq.argum[1] = "1"

OpenWithParm(w_busc_duchacontrol, lstr_busq)

lstr_busq	= Message.PowerObjectParm

IF lstr_busq.argum[2] <> '' THEN
	
	dw_2.SetItem(il_fila,"duch_codigo",Integer(lstr_busq.argum[1]))
	dw_2.SetItem(il_fila,"codu_nropos",Integer(lstr_busq.argum[2]))
	dw_2.SetItem(il_fila,"codu_fecini",Date(lstr_busq.argum[3]))
	dw_2.SetItem(il_fila,"codu_horini",Time(lstr_busq.Argum[4]))
	dw_2.SetColumn("bpdu_canbin")	
	
	istr_mant.argumento[1] = lstr_busq.argum[1]
	istr_mant.argumento[2] = lstr_busq.argum[2]
	istr_mant.argumento[3] = lstr_busq.argum[3]
	istr_mant.argumento[4] = lstr_busq.argum[4]
   istr_mant.argumento[6] = lstr_busq.argum[3]
	istr_mant.argumento[7] = lstr_busq.argum[4]
	
	ld_FechaSistema	=	Date(F_FechaHora())
	lt_HoraSistema		=	Time(F_FechaHora())
	
	dw_2.SetItem(il_Fila,"bpdu_feceve",Date(ld_FechaSistema))
	dw_2.SetItem(il_Fila,"bpdu_horeve",Time(lt_HoraSistema))
	
	IF iuo_duchacontrol.Captura_BinsDuchados(Integer(lstr_busq.argum[1]),Integer(lstr_busq.argum[2]),&
														Date(lstr_busq.argum[3]),Time(lstr_busq.Argum[4]),&
														SQLCA) THEN

		dw_2.SetItem(il_fila,"bpdu_canbin",iuo_duchacontrol.ii_TotatBinsDuchados)
	
	 Habilitabotton("duch_codigo")   
	END IF
ELSE			
	dw_1.SetFocus()
END IF

RETURN
end subroutine

on w_maed_spro_bitaproducducha.create
call super::create
end on

on w_maed_spro_bitaproducducha.destroy
call super::destroy
end on

event open;call super::open;
/*
istr_mant.argumento[1] = Código Ducha
istr_mant.argumento[2] = Nº Estanque
istr_mant.argumento[3] = Fecha Inicio
istr_mant.argumento[4] = Hora Inicio
istr_mant.argumento[5] = Tipo Envase
istr_mant.argumento[6] = Fecha Evento
istr_mant.argumento[7] = Hora Evento
*/
istr_mant.argumento[5] = "2"

buscar  = "Código Ducha:Nduch_codigo,Código Estanque:Ncodu_nropos"
ordenar = "Código Ducha:duch_codigo,Código Estanque:codu_nropos"

iuo_duchacontrol	=	Create uo_duchacontrol

end event

event ue_recuperadatos();call super::ue_recuperadatos;Long		ll_fila_d, ll_fila_e, respuesta
Date		ld_Fecha, ld_FechaEv
Time	 	lt_HoraEv, lt_Hora

ld_Fecha	=	Date(istr_mant.Argumento[3])
lt_Hora		=	Time(istr_mant.Argumento[4])

ld_FechaEv	=	Date(istr_mant.Argumento[6])
lt_HoraEv	=	Time(istr_mant.Argumento[7])

DO
	dw_2.SetRedraw(False)
	dw_2.Reset()

	ll_fila_e	= dw_2.Retrieve(Integer(istr_mant.Argumento[1]), &
										Integer(istr_mant.Argumento[2]), &
										ld_Fecha, lt_Hora)

	IF ll_fila_e = -1 THEN
		respuesta = MessageBox("Error en Base de Datos", "No es posible conectar la Base de Datos.", &
										Information!, RetryCancel!)
	ELSE
		HabilitaEncab(False)
		
		DO
			ll_fila_d	= dw_1.Retrieve(Integer(istr_mant.Argumento[1]), &
										Integer(istr_mant.Argumento[2]), &
										ld_Fecha, lt_Hora, 2, ld_FechaEv, lt_HoraEv)

			IF ll_fila_d = -1 THEN
				respuesta = MessageBox("Error en Base de Datos", "No es posible conectar la Base de Datos.", &
												Information!, RetryCancel!)
			ELSE
				pb_eliminar.Enabled  = True
				pb_grabar.Enabled		= True
				pb_ins_det.Enabled	= True
				pb_imprimir.Enabled	= True
				
				IF ll_fila_d > 0 THEN
					pb_eli_det.Enabled	= True
					dw_1.SetRow(1)
					//dw_1.SelectRow(1,True)
					dw_1.SetFocus()
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
end event

event ue_borra_detalle();call super::ue_borra_detalle;IF ii_seleccion = 0 THEN
   IF dw_1.rowcount() < 1 THEN RETURN

   SetPointer(HourGlass!)
 
   ib_borrar = True
   w_main.SetMicroHelp("Validando la eliminación de detalle...")

   Message.DoubleParm = 0

   This.TriggerEvent ("ue_validaborrar_detalle")

   IF Message.DoubleParm = -1 THEN RETURN

   IF MessageBox("Eliminación de Registro", "Está seguro de Eliminar este Registro", Question!, YesNo!) = 1 THEN
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
	   END IF
   END IF
ELSE 
  
       SetPointer(HourGlass!)
 
       ib_borrar = True
       w_main.SetMicroHelp("Validando la eliminación de detalle...")

       Message.DoubleParm = 0

       This.TriggerEvent ("ue_validaborrar_detalle")
      
       IF Message.DoubleParm = -1 THEN RETURN
END IF





end event

event ue_seleccion();call super::ue_seleccion;str_busqueda	lstr_busq

lstr_busq.argum[1] = "2"

OpenWithParm(w_busc_relleno_ducha, lstr_busq)

lstr_busq	= Message.PowerObjectParm

IF lstr_busq.argum[1] <> "" THEN
	istr_mant.argumento[1] = lstr_busq.argum[1]
	istr_mant.argumento[2] = lstr_busq.argum[2]
	istr_mant.argumento[3] = lstr_busq.argum[3]
	istr_mant.argumento[4] = lstr_busq.argum[4]
	istr_mant.argumento[5] = lstr_busq.argum[5]
	istr_mant.argumento[6] = lstr_busq.argum[6]
	istr_mant.argumento[7] = lstr_busq.argum[7]
	pb_ins_det.Enabled	  = True

	This.TriggerEvent("ue_recuperadatos")
ELSE
	pb_buscar.SetFocus()
END IF
end event

event ue_nuevo_detalle();istr_mant.borra 	= False
istr_mant.agrega	= True
	
IF ii_seleccion	=	0	THEN	
	IF il_fila > 0 THEN
		pb_eliminar.Enabled	= True
		pb_grabar.Enabled		= True
	END IF
	
	il_fila = dw_1.InsertRow(0)
	
	dw_1.ScrollToRow(il_fila)
	dw_1.SetRow(il_fila)
	dw_1.SetFocus()
	dw_1.SetItem(il_fila,"bpdu_tipoev",2)
	dw_1.SetColumn("prdu_codigo")
	
ELSE
	IF il_fila > 0 THEN
		pb_eliminar.Enabled	= True
		pb_grabar.Enabled		= True
	END IF
	

END IF



end event

event ue_nuevo();call super::ue_nuevo;Date	ld_Fecha
Time	lt_Hora
HabilitaEncab(True)

dw_1.Retrieve(0, 0, ld_Fecha, lt_Hora, 2, ld_Fecha, lt_Hora)
dw_2.SetColumn("duch_codigo")
end event

event ue_modifica_detalle();call super::ue_modifica_detalle;//IF dw_1.RowCount() > 0 THEN
//	istr_mant.agrega	= False
//	istr_mant.borra	= False
//
//	OpenWithParm(iw_mantencion, istr_mant)
//END IF
end event

event ue_antesguardar();Long		ll_fila
Date		ld_FechaEv
Time		lt_HoraEv

il_fila	=	1

IF Not ExisteDucha() THEN
	MessageBox("Error de Consistencia","Las datos ingresados no corresponden a ninguna ducha")
	Message.DoubleParm = -1
	RETURN
END IF

ld_FechaEv	=	Date(Mid(istr_mant.Argumento[6], 1, 10))
lt_HoraEv	=	Time(Mid(istr_mant.Argumento[7], 12))

dw_2.SetItem(1, "bpdu_horeve", lt_HoraEv)

DO WHILE il_fila <= dw_1.RowCount()
	IF dw_1.GetItemStatus(il_fila, 0, Primary!) = New! THEN
		dw_1.DeleteRow(il_fila)
	ELSEIF dw_1.GetItemStatus(il_fila, 0, Primary!) = New! OR dw_1.GetItemStatus(il_fila, 0, Primary!) = NewModified! THEN
			TriggerEvent("ue_validaregistro")

			dw_1.SetItem(il_fila, "duch_codigo", dw_2.Object.duch_codigo[1])
			dw_1.SetItem(il_fila, "codu_nropos", dw_2.Object.codu_nropos[1])
			dw_1.SetItem(il_fila, "codu_fecini", dw_2.Object.codu_fecini[1])
			dw_1.SetItem(il_fila, "codu_horini", dw_2.Object.codu_horini[1])
			dw_1.SetItem(il_fila, "bpdu_tipoev", 2)
			dw_1.SetItem(il_fila, "bpdu_feceve", dw_2.Object.bpdu_feceve[1])
			dw_1.SetItem(il_fila, "bpdu_horeve", lt_HoraEv)

	END IF
	il_fila ++
LOOP

IF dw_2.GetItemStatus(1, 0, Primary!) = New! OR &
	dw_2.GetItemStatus(1, 0, Primary!) = NewModified! THEN

	dw_2.SetItem(1, "bpdu_tipoev", 2)
END IF
end event

type dw_1 from w_mant_encab_deta_csd`dw_1 within w_maed_spro_bitaproducducha
integer x = 9
integer y = 1036
integer width = 2615
integer height = 848
integer taborder = 100
string title = "Documentos Plantas"
string dataobject = "dw_mues_spro_bitaproducduchadet"
end type

event dw_1::itemchanged;call super::itemchanged;String	ls_null

SetNull(ls_null)

CHOOSE CASE GetColumnName()

	CASE "prdu_codigo"	
		IF Duplicado(data) THEN
			This.SetItem(il_fila, "prdu_codigo",ls_null)
			RETURN 1
		END IF
		
	CASE "bpdd_cantid"
		IF NOT This.uf_validate(row) THEN
			This.SetItem(row, "bpdd_cantid", Dec(ls_Null))
			RETURN 1
		END IF
END CHOOSE
end event

event dw_1::clicked;//
end event

event dw_1::doubleclicked;//
end event

event dw_1::rowfocuschanged;//
end event

event dw_1::getfocus;//
end event

event dw_1::constructor;call super::constructor;This.uf_add_validation('bpdd_cantid > 0 and bpdd_cantid < 99999.999','Valor fuera de rango para la cantidad')
end event

type dw_2 from w_mant_encab_deta_csd`dw_2 within w_maed_spro_bitaproducducha
integer x = 78
integer y = 72
integer width = 1769
integer height = 748
integer taborder = 10
string dataobject = "dw_mant_bitaproducduchadet"
end type

event dw_2::clicked;call super::clicked;//
end event

event dw_2::doubleclicked;call super::doubleclicked;//
end event

event dw_2::itemchanged;String	ls_Campo
Integer  li_Null

ls_Campo = dwo.name
SetNull(li_Null)

CHOOSE CASE ls_Campo

	CASE "duch_codigo", "codu_nropos", "codu_fecini", "codu_horini"
	   ExisteEncabezado(ls_Campo, Data)

	CASE "duch_codigo"
		istr_Mant.argumento[1] = Data

	CASE "codu_nropos"
		istr_Mant.argumento[2] = Data

	CASE "codu_fecini"
		istr_Mant.argumento[3] = Data

	CASE "codu_horini"
		istr_Mant.argumento[4] = Data


	CASE "bpdu_feceve"
		istr_Mant.argumento[6] = Data
	   ExisteEncabezado(ls_Campo, Data)

	CASE "bpdu_horeve"
		istr_Mant.argumento[7] = Data
	   ExisteEncabezado(ls_Campo, Data)

END CHOOSE

HabilitaBotton(dwo.name)
end event

event dw_2::buttonclicked;
CHOOSE CASE dwo.Name
	CASE "buscaducha"
		BuscaDucha()
END CHOOSE		

end event

type pb_nuevo from w_mant_encab_deta_csd`pb_nuevo within w_maed_spro_bitaproducducha
integer x = 2478
integer y = 316
integer taborder = 30
end type

type pb_eliminar from w_mant_encab_deta_csd`pb_eliminar within w_maed_spro_bitaproducducha
integer x = 2478
integer y = 496
integer taborder = 40
end type

type pb_grabar from w_mant_encab_deta_csd`pb_grabar within w_maed_spro_bitaproducducha
integer x = 2478
integer y = 672
integer taborder = 50
end type

type pb_imprimir from w_mant_encab_deta_csd`pb_imprimir within w_maed_spro_bitaproducducha
boolean visible = false
integer x = 2478
integer y = 852
integer taborder = 60
end type

type pb_salir from w_mant_encab_deta_csd`pb_salir within w_maed_spro_bitaproducducha
integer x = 2478
integer y = 1036
integer taborder = 90
end type

type pb_ins_det from w_mant_encab_deta_csd`pb_ins_det within w_maed_spro_bitaproducducha
integer x = 2491
integer y = 1328
integer taborder = 70
end type

type pb_eli_det from w_mant_encab_deta_csd`pb_eli_det within w_maed_spro_bitaproducducha
integer x = 2491
integer y = 1504
integer taborder = 80
end type

type pb_buscar from w_mant_encab_deta_csd`pb_buscar within w_maed_spro_bitaproducducha
integer x = 2482
integer y = 132
integer taborder = 20
end type

