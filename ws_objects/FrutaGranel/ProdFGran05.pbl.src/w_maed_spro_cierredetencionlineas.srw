$PBExportHeader$w_maed_spro_cierredetencionlineas.srw
forward
global type w_maed_spro_cierredetencionlineas from w_mant_encab_deta_csd
end type
end forward

global type w_maed_spro_cierredetencionlineas from w_mant_encab_deta_csd
integer width = 2752
integer height = 2232
string title = "CIERRE DETENCION LINEA"
string menuname = ""
event ue_validaregistro ( )
end type
global w_maed_spro_cierredetencionlineas w_maed_spro_cierredetencionlineas

type variables
Integer	ii_seleccion, il_filla
DateTime idt_fecini, idt_horini

uo_especie				iuo_especie
uo_plantadesp			iuo_plantadesp
uo_lineapacking		iuo_lineapacking

DataWindowChild		idwc_especie, idwc_planta,idwc_linea, idwc_area
DataWindow				dw_3, dw_4
end variables

forward prototypes
protected function boolean wf_actualiza_db (boolean borrando)
public subroutine habilita_encab (boolean habilita)
public function boolean validaminutos ()
public subroutine habilitaingreso (string columna)
public function boolean existeencabezado (string as_columna, string as_valor)
end prototypes

event ue_validaregistro();//Integer	li_cont,li_cont1,ll_fila
//String	ls_mensaje,ls_colu[],ls_colu1[]
//	
//IF dw_3.GetItemStatus(il_Fila, 0, Primary!) = NewModified! THEN
//	IF Isnull(dw_3.Object.rdla_turno[il_fila]) OR dw_3.Object.rdla_turno[il_fila] = 0 THEN
//		li_cont ++
//		ls_mensaje 			= ls_mensaje + "~nCódigo Turno"
//		ls_colu[li_cont]	= "rdla_turno"
//	END IF
//		
//	IF li_cont > 0 THEN
//		MessageBox("Error de Consistencia", "Falta el ingreso de :" + ls_mensaje + ".", StopSign!, Ok!)
//		dw_3.SetColumn(ls_colu[1])
//		dw_3.SetFocus()
//		Message.DoubleParm = -1
//	END IF
//END IF
//
end event

protected function boolean wf_actualiza_db (boolean borrando);Boolean	lb_AutoCommit, lb_Retorno

IF Not dw_2.uf_check_required(0) THEN RETURN False


lb_AutoCommit		=	sqlca.AutoCommit
sqlca.AutoCommit	=	False

IF Borrando THEN
			IF dw_2.Update(True, False) = 1 THEN
				Commit;
				
				IF sqlca.SQLCode <> 0 THEN
					F_ErrorBaseDatos(sqlca, This.Title)
				ELSE
					lb_Retorno	=	True
					dw_2.ResetUpdate()
				END IF
			ELSE
				F_ErrorBaseDatos(sqlca, This.Title)
				
				RollBack;
			END IF
ELSE
	IF dw_2.Update(True, False) = 1 THEN
				Commit;
				
				IF sqlca.SQLCode <> 0 THEN
					F_ErrorBaseDatos(sqlca, This.Title)
				ELSE
					lb_Retorno	=	True
					dw_2.ResetUpdate()
				END IF
			ELSE
				F_ErrorBaseDatos(sqlca, This.Title)
				
				RollBack;
			END IF
END IF

sqlca.AutoCommit	=	lb_AutoCommit

RETURN lb_Retorno
end function

public subroutine habilita_encab (boolean habilita);IF Habilita THEN
	dw_2.Object.plde_codigo.Protect				=	0
	dw_2.Object.line_codigo.Protect				=	0
	dw_2.Object.espe_codigo.Protect				=	0
	dw_2.Object.edla_fecpro.Protect				=	0
	dw_2.Object.edla_estado.Protect				=	0
	dw_2.Object.plde_codigo.BackGround.Color	=	RGB(255,255,255)
	dw_2.Object.line_codigo.BackGround.Color	=	RGB(255,255,255)
	dw_2.Object.espe_codigo.BackGround.Color	=	RGB(255,255,255)
	dw_2.Object.edla_fecpro.BackGround.Color	=	RGB(255,255,255)
	dw_2.Object.edla_estado.BackGround.Color	=	RGB(192,192,192)
ELSE
	dw_2.Object.plde_codigo.Protect				=	1
	dw_2.Object.line_codigo.Protect				=	1
	dw_2.Object.espe_codigo.Protect				=	1
	dw_2.Object.edla_fecpro.Protect				=	1
	dw_2.Object.edla_estado.Protect				=	0
	dw_2.Object.edla_fecpro.BackGround.Color	=	RGB(192,192,192)
	dw_2.Object.plde_codigo.BackGround.Color	=	RGB(192,192,192)
	dw_2.Object.line_codigo.BackGround.Color	=	RGB(192,192,192)
	dw_2.Object.espe_codigo.BackGround.Color	=	RGB(192,192,192)
	dw_2.Object.edla_fecpro.BackGround.Color	=	RGB(192,192,192)
	dw_2.Object.edla_estado.BackGround.Color	=	RGB(255,255,255)
END IF
end subroutine

public function boolean validaminutos ();Boolean	lb_Retorno
Integer	li_MinInf,li_MinJus

IF dw_1.RowCount() > 0 THEN 
	li_MinInf = dw_1.Object.total_info[1]
	li_MinJus = dw_1.Object.total_justi[1]
	
	IF li_MinInf <> li_MinJus THEN
		MessageBox("Atención","No se Puede Cerrar la Detención Línea" + &
					  "~r~rLos Minutos Justificados Deben Ser Igual " + &
					  "~r~ra Los Informados",Information!, Ok!)
		Return True
	ELSE
		pb_imprimir.Enabled	=	True	
	END IF	
END IF

REturn False
end function

public subroutine habilitaingreso (string columna);Boolean	lb_Estado = True
Date  	ldt_Hora
	
IF IsNull(dw_2.Object.plde_codigo[dw_2.GetRow()]) OR dw_2.Object.plde_codigo[dw_2.GetRow()] = 0 THEN
	lb_Estado	=	False
END IF

IF IsNull(dw_2.Object.line_codigo[dw_2.GetRow()]) OR dw_2.Object.line_codigo[dw_2.GetRow()] = 0 THEN
	lb_Estado	=	False
END IF

IF IsNull(dw_2.Object.rdla_turno[dw_2.GetRow()])  OR dw_2.Object.rdla_turno[dw_2.GetRow()]  = 0 THEN
	lb_Estado	=	False 
END IF


end subroutine

public function boolean existeencabezado (string as_columna, string as_valor);Boolean	lb_Retorno
Integer	li_Planta, li_Linea, li_Especie, li_Cantidad, li_Turno
Date		ldt_Fecha, ldt_FechaNula
String   ls_fecha, ls_hora

li_Planta		=	dw_2.Object.plde_codigo[1]
li_Linea			=	dw_2.Object.line_codigo[1]
li_Especie		=	dw_2.Object.espe_codigo[1]
ldt_fecha		=	dw_2.Object.edla_fecpro[1]
li_Turno			=	dw_2.Object.rdla_turno[1]

CHOOSE CASE as_Columna	
	
	CASE "line_codigo"
		li_Linea		=	Integer(as_valor)
	CASE "espe_codigo"
		li_Especie	=	Integer(as_valor)
	CASE "edla_fecpro"
		ldt_Fecha	=	Date(as_Valor)		
	CASE "rdla_turno"
		li_Turno		= 	Integer(string(as_valor, "dd/mm/yyyy"))
END CHOOSE

SELECT	Count(*)
	INTO	:li_Cantidad
	FROM	dba.spro_detencionlineaenca
	WHERE	plde_codigo	=	:li_Planta
	AND   line_codigo	=	:li_Linea
	AND	espe_codigo	=	:li_Especie
	AND	edla_fecpro	=	:ldt_Fecha 
	AND   rdla_Turno	=	:li_Turno
	AND 	edla_estado	=	1;

IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de Tabla Spro_detencionlineaenca ")
	
	RETURN TRUE
ELSEIF sqlca.SQLCode <> 100 and li_cantidad>0 THEN
	istr_mant.argumento[1]	=	String(li_Planta)
	istr_mant.argumento[2]	=	String(li_Linea)
	istr_mant.argumento[3]	=	String(li_Especie)
	istr_mant.argumento[4]	=	String(ldt_Fecha)
	istr_Mant.Argumento[5]	=	String(li_Turno)
	This.TriggerEvent("ue_recuperadatos")
	
	lb_Retorno	=	True
ELSE
	lb_Retorno	=	False
END IF

RETURN lb_Retorno
end function

event ue_seleccion();call super::ue_seleccion;OpenWithParm(w_busc_detencionlinea, istr_busq)

istr_busq	= Message.PowerObjectParm

IF istr_busq.argum[2] <> "" THEN
	if  istr_busq.argum[1] = String(gstr_ParamPlanta.CodigoPlanta) then
		istr_mant.argumento[1] = String(gstr_ParamPlanta.CodigoPlanta)
		istr_mant.argumento[2] = istr_busq.argum[2]
		istr_mant.argumento[3] = istr_busq.argum[3]
		istr_mant.argumento[4] = istr_busq.argum[4]
		istr_mant.argumento[5] = istr_busq.argum[5]
	
		TriggerEvent("ue_recuperadatos")
	else
		messagebox("No se pueden mostrar los datos","No es posible mostrar datos de otra planta ~r Seleccione un registro correspondiente" )
	end if
ELSE
	pb_buscar.SetFocus()
END IF
end event

event ue_recuperadatos();call super::ue_recuperadatos;Long		ll_fila_d, ll_fila_e, respuesta,ll_fila_f
Date		ld_Fecha
Time		lt_Hora

ld_Fecha		=	Date(Mid(istr_mant.Argumento[4], 1, 10))
lt_Hora		=	Time(Mid(istr_mant.Argumento[4], 12))

DO
	dw_2.SetRedraw(False)
	
	ll_fila_e	=	dw_2.Retrieve(Integer(istr_mant.Argumento[1]), &
										Integer(istr_mant.Argumento[2]), &
										Integer(istr_mant.Argumento[3]),&
										ld_Fecha,Integer(istr_Mant.Argumento[5]))
	
	IF ll_fila_e = -1 THEN
		respuesta = MessageBox(	"Error en Base de Datos", "No es posible conectar la Base de Datos.", &
										Information!, RetryCancel!)
	ELSE
		DO
			ll_fila_d	=	dw_1.Retrieve(Integer(istr_mant.Argumento[1]), &
										Integer(istr_mant.Argumento[2]), &
										Integer(istr_mant.Argumento[3]),&
										ld_Fecha,Integer(istr_Mant.Argumento[5]))
			IF ll_fila_d = -1 THEN
				respuesta = MessageBox(	"Error en Base de Datos", "No es posible conectar la Base de Datos.", &
												Information!, RetryCancel!)
			ELSE	
					IF ll_fila_d > 0 OR ll_fila_e > 0 OR ll_fila_f >0 THEN
						pb_eli_det.Enabled	=	False
						pb_imprimir.Enabled	=	False			
						Habilita_Encab(False)
					END IF
			END IF
		LOOP WHILE respuesta = 1
		IF respuesta = 2 THEN Close(This)
	END IF	
	dw_2.SetRedraw(True)
LOOP WHILE respuesta = 1

IF respuesta = 2 THEN Close(This)
end event

event ue_imprimir();SetPointer(HourGlass!)

Long		fila
str_info	lstr_info

lstr_info.titulo	= "CIERRE DETENCION LINEAS"
lstr_info.copias	= 1

OpenWithParm(vinf,lstr_info)

vinf.dw_1.DataObject = "dw_info_detencionlinea_cierre"

vinf.dw_1.SetTransObject(sqlca)

fila = vinf.dw_1.Retrieve(Integer(istr_mant.argumento[1]),0, &
								  Integer(istr_mant.argumento[2]),Integer(istr_mant.argumento[3]),&
								  Date(Mid(istr_mant.argumento[4],1,10)),Integer(istr_Mant.Argumento[5]))

IF fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ELSEIF fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", &
					StopSign!, Ok!)
ELSE
	F_Membrete(vinf.dw_1)
	vinf.dw_1.Modify('DataWindow.Print.Preview = Yes')
	vinf.dw_1.Modify('DataWindow.Print.Preview.Zoom = 75')

	vinf.Visible	= True
	vinf.Enabled	= True
END IF

SetPointer(Arrow!)
end event

event ue_borra_detalle();//IF istr_Mant.Solo_Consulta THEN RETURN
//
//
//Integer	li_tabpage
//
//li_tabpage			=	tab_1.SelectedTab
//
//CHOOSE CASE li_tabpage
//	CASE 1		
//		IF dw_3.rowcount() < 1 THEN RETURN
//		
//			SetPointer(HourGlass!)
//		 
//			ib_borrar = True
//			w_main.SetMicroHelp("Validando la eliminación de detalle...")
//		
//			Message.DoubleParm = 0
//		
//			This.TriggerEvent ("ue_validaborrar_detalle")
//		
//			IF Message.DoubleParm = -1 THEN RETURN
//		
//			IF MessageBox("Eliminación de Registro", "Está seguro de Eliminar este Registro", Question!, YesNo!) = 1 THEN
//				IF dw_3.DeleteRow(0) = 1 THEN
//					ib_borrar = False
//					w_main.SetMicroHelp("Borrando Registro...")
//					SetPointer(Arrow!)
//				ELSE
//					ib_borrar = False
//					MessageBox(This.Title,"No se puede borrar actual registro.")
//				END IF
//		
//				IF dw_3.RowCount() = 0 THEN
//					pb_eliminar.Enabled = False
//				ELSE
//					il_fila = dw_3.GetRow()
//				END IF
//			END IF
//
//		
//	CASE 2
//		IF dw_4.rowcount() < 1 THEN RETURN
//
//       SetPointer(HourGlass!)
// 
//       ib_borrar = True
//       w_main.SetMicroHelp("Validando la eliminación de detalle...")
//
//       Message.DoubleParm = 0
//
//       This.TriggerEvent ("ue_validaborrar_detalle")
//      
//       IF Message.DoubleParm = -1 THEN RETURN
//      
//      IF MessageBox("Eliminación de Registro", "Está seguro de Eliminar este Registro", Question!, YesNo!) = 1 THEN
//         IF dw_4.DeleteRow(0) = 1 THEN
//	         ib_borrar = False
//		      w_main.SetMicroHelp("Borrando Registro...")
//		      SetPointer(Arrow!)
//         ELSE
//	      	ib_borrar = False
//	   	   MessageBox(This.Title,"No se puede borrar actual registro.")
//         END IF
//
//         IF dw_4.RowCount() = 0 THEN
// 	         pb_eliminar.Enabled = False
//         ELSE
//		      il_fila = dw_4.GetRow()
//         END IF
//     END IF
//
//		
//END CHOOSE
//
//
////IF ii_seleccion = 0 THEN
////   IF dw_3.rowcount() < 1 THEN RETURN
////
////   SetPointer(HourGlass!)
//// 
////   ib_borrar = True
////   w_main.SetMicroHelp("Validando la eliminación de detalle...")
////
////   Message.DoubleParm = 0
////
////   This.TriggerEvent ("ue_validaborrar_detalle")
////
////   IF Message.DoubleParm = -1 THEN RETURN
////
////   IF MessageBox("Eliminación de Registro", "Está seguro de Eliminar este Registro", Question!, YesNo!) = 1 THEN
////	   IF dw_3.DeleteRow(0) = 1 THEN
////	   	ib_borrar = False
////	   	w_main.SetMicroHelp("Borrando Registro...")
////		   SetPointer(Arrow!)
////	   ELSE
////		   ib_borrar = False
////		   MessageBox(This.Title,"No se puede borrar actual registro.")
////	   END IF
////
////      IF dw_3.RowCount() = 0 THEN
////		   pb_eliminar.Enabled = False
////	   ELSE
////		   il_fila = dw_3.GetRow()
////	   END IF
////   END IF
////ELSE 
////     IF dw_4.rowcount() < 1 THEN RETURN
////
////       SetPointer(HourGlass!)
//// 
////       ib_borrar = True
////       w_main.SetMicroHelp("Validando la eliminación de detalle...")
////
////       Message.DoubleParm = 0
////
////       This.TriggerEvent ("ue_validaborrar_detalle")
////      
////       IF Message.DoubleParm = -1 THEN RETURN
////      
////      IF MessageBox("Eliminación de Registro", "Está seguro de Eliminar este Registro", Question!, YesNo!) = 1 THEN
////         IF dw_4.DeleteRow(0) = 1 THEN
////	         ib_borrar = False
////		      w_main.SetMicroHelp("Borrando Registro...")
////		      SetPointer(Arrow!)
////         ELSE
////	      	ib_borrar = False
////	   	   MessageBox(This.Title,"No se puede borrar actual registro.")
////         END IF
////
////         IF dw_4.RowCount() = 0 THEN
//// 	         pb_eliminar.Enabled = False
////         ELSE
////		      il_fila = dw_4.GetRow()
////         END IF
////     END IF
////END IF
end event

on w_maed_spro_cierredetencionlineas.create
call super::create
end on

on w_maed_spro_cierredetencionlineas.destroy
call super::destroy
end on

event ue_modifica_detalle();call super::ue_modifica_detalle;//IF istr_Mant.Solo_Consulta THEN RETURN
//
//IF dw_1.RowCount() > 0 THEN
//	istr_mant.agrega	= False
//	istr_mant.borra	= False
//
//	OpenWithParm(iw_mantencion, istr_mant)
//END IF
end event

event open;/*
Argumentos

istr_mant.argumento[1]	=	Planta
istr_mant.argumento[2] 	=	Linea
istr_mant.argumento[3]	=	Especie
istr_mant.argumento[4]	=	Fecha Proceso
istr_Mant.Argumento[5]	=	Turno
*/

x				= 0
y				= 0
This.Height	= 2020
im_menu		= m_principal

This.Icon									=	Gstr_apl.Icono
This.ParentWindow().ToolBarVisible	=	True
im_menu.Item[1].Item[6].Enabled		=	True
im_menu.Item[7].Visible					=	True

dw_1.SetTransObject(sqlca)
dw_2.SetTransObject(sqlca)
dw_1.Modify("datawindow.message.title='Error '+ is_titulo")
dw_1.SetRowFocusIndicator(Hand!)
dw_1.Modify("DataWindow.Footer.Height = 110")

istr_mant.dw						=	dw_1
istr_mant.UsuarioSoloConsulta	=	OpcionSoloConsulta()

pb_nuevo.PostEvent(Clicked!)
iuo_especie				=	Create uo_especie
iuo_plantadesp			=	Create uo_plantadesp
iuo_lineapacking		=	Create uo_lineapacking
//Planta
dw_2.GetChild("plde_codigo", idwc_planta)
idwc_planta.SetTransObject(sqlca)
IF idwc_planta.Retrieve(gi_codexport) = 0 THEN
	MessageBox("Atención","Falta Registrar Plantas")
ELSE
	idwc_Planta.InsertRow(0)
END IF

//Linea Packing
dw_2.GetChild("line_codigo", idwc_linea)
idwc_linea.SetTransObject(sqlca)
IF idwc_linea.Retrieve(0) = 0 THEN
	MessageBox("Atención","Falta Registrar Líneas Packing")
ELSE
	idwc_linea.InsertRow(0)
END IF

//Especie
dw_2.GetChild("espe_codigo", idwc_especie)
idwc_especie.SetTransObject(sqlca)
IF idwc_especie.Retrieve(gstr_parempresa.empr_codexp) = 0 THEN
	MessageBox("Atención","Falta Registrar Especies")
ELSE
	idwc_especie.InsertRow(0)
END IF


istr_mant.argumento[1]	=	String(gstr_ParamPlanta.CodigoPlanta)

end event

event ue_antesguardar();//Long		ll_Fila
//String   ls_fechaini, ls_horaini,ls_fechater, ls_horater
//
//
//FOR il_fila = 1 TO dw_3.RowCount()
//	
//	IF dw_3.GetItemStatus(il_fila, 0, Primary!) = New! THEN
//		
//		dw_3.DeleteRow(il_fila)
//		
//	ELSEIF dw_3.GetItemStatus(il_fila, 0, Primary!) = NewModified! THEN
//		
//		TriggerEvent("ue_validaregistro")				
//
//		dw_3.SetItem(il_fila, "plde_codigo", dw_2.Object.plde_codigo[1])
//		dw_3.SetItem(il_fila, "line_codigo", dw_2.Object.line_codigo[1])
//		dw_3.SetItem(il_fila, "espe_codigo", dw_2.Object.espe_codigo[1])		
//		dw_3.SetItem(il_fila, "edla_fecpro", dw_2.Object.edla_fecpro[1])
//		
//		/* Compone Fecha de Hora Inicio */
//		ls_fechaini	=  Mid(String(dw_3.Object.fecha_ini[il_fila]),1,10)
//		ls_horaini	=  Mid(String(dw_3.Object.hdla_horini[il_fila]),12,5)
//		ls_horaini  =  ls_horaini+':00'
//		dw_3.Object.hdla_horini[il_fila]	=	DateTime(Date(ls_fechaini),Time(ls_Horaini))
//		
//		/* Compone Fecha de Hora Termino */
//		ls_fechater	=  Mid(String(dw_3.Object.fecha_ter[il_fila]),1,10)
//		ls_horater	=  Mid(String(dw_3.Object.hdla_horter[il_fila]),12,5)
//		ls_horater  =  ls_horater+':00'
//		dw_3.Object.hdla_horter[il_fila]	=	DateTime(Date(ls_fechater),Time(ls_Horater))
//		
//	END IF
//NEXT
//
//FOR il_fila = 1 TO dw_4.RowCount()
//	
//	IF dw_4.GetItemStatus(il_fila, 0, Primary!) = New! THEN
//		
//		dw_4.DeleteRow(il_fila)
//		
//	ELSEIF dw_4.GetItemStatus(il_fila, 0, Primary!) = NewModified! THEN
//		
//		TriggerEvent("ue_validaregistro")				
//
//		dw_4.SetItem(il_fila, "plde_codigo", dw_2.Object.plde_codigo[1])
//		dw_4.SetItem(il_fila, "line_codigo", dw_2.Object.line_codigo[1])
//		dw_4.SetItem(il_fila, "espe_codigo", dw_2.Object.espe_codigo[1])		
//		dw_4.SetItem(il_fila, "edla_fecpro", dw_2.Object.edla_fecpro[1])
//	END IF
//NEXT
//		
//		
end event

event ue_nuevo();Long		ll_modif1, ll_modif2,ll_modif3
String   ls_hora
ib_ok	= True

istr_mant.Solo_Consulta	=	istr_mant.UsuarioSoloConsulta

IF Not istr_mant.Solo_Consulta THEN
	CHOOSE CASE wf_modifica()
		CASE -1
			ib_ok = False
		CASE 0
			ll_modif1	=	dw_1.GetNextModified(0, Primary!)
			ll_modif2	=	dw_2.GetNextModified(0, Primary!)
					
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

pb_grabar.Enabled			=	False
dw_2.Enabled				=	True

dw_2.SetRedraw(False)
dw_2.Reset()

dw_2.Object.edla_fecpro[1]		=	Date(Today())
//ls_hora                       =  MID(String(Now()),1,5)
//ls_hora								=	ls_hora+':00'
//dw_2.Object.edla_fecpro[1]		=	Date(Today())


dw_2.SetRedraw(True)
Habilita_Encab(True)
dw_2.SetFocus()
dw_2.SetItem(1,"plde_codigo",gstr_ParamPlanta.CodigoPlanta)
dw_2.Object.plde_codigo.Protect				=	0
dw_2.Object.plde_codigo.BackGround.Color	=	RGB(192,192,192)

dw_2.SetColumn("line_codigo")
end event

event ue_validaborrar();call super::ue_validaborrar;//
end event

event ue_borrar();//IF istr_Mant.Solo_Consulta THEN RETURN
//
//IF dw_3.RowCount() < 1 OR dw_2.Rowcount() < 1 THEN RETURN
//
//SetPointer(HourGlass!)
//
//ib_borrar = True
//w_main.SetMicroHelp("Validando la eliminación...")
//
//Message.DoubleParm = 0
//
//This.TriggerEvent ("ue_validaborrar")
//
//IF Message.DoubleParm = -1 THEN RETURN
//
//IF dw_3.RowCount() > 0 THEN dw_3.RowsMove(1, dw_3.RowCount(), Primary!, dw_3, 1, Delete!)
//
//IF dw_1.RowCount() > 0 THEN dw_1.RowsMove(1, dw_1.RowCount(), Primary!, dw_1, 1, Delete!)
//
//IF dw_2.DeleteRow(0) = 1 THEN
//	ib_borrar	=	False
//	
//	w_main.SetMicroHelp("Borrando Registro...")
//	
//	IF wf_actualiza_db(True) THEN
//		w_main.SetMicroHelp("Registro Borrado...")
//		This.TriggerEvent("ue_nuevo")
//		SetPointer(Arrow!)
//	ELSE
//		w_main.SetMicroHelp("Registro no Borrado...")
//	END IF
//END IF
end event

event ue_listo();//
end event

type dw_1 from w_mant_encab_deta_csd`dw_1 within w_maed_spro_cierredetencionlineas
integer x = 87
integer y = 1124
integer width = 1810
integer height = 816
integer taborder = 30
boolean titlebar = false
string title = "Hora"
string dataobject = "dw_cierrelineadtencion"
boolean hscrollbar = false
end type

event dw_1::rowfocuschanged;ib_datos_ok = True

IF rowcount() < 1 OR getrow() = 0 OR ib_borrar THEN
	ib_datos_ok = False
ELSE
	il_fila = getrow()
	This.SelectRow(0,False)
	This.SelectRow(il_fila,False)
END IF

RETURN 0
end event

event dw_1::clicked;IF Row > 0 THEN
	il_fila = Row
END IF

RETURN 0
end event

event dw_1::getfocus;RETURN 0
end event

event type long dw_1::ue_seteafila(unsignedlong wparam, long lparam);RETURN 0
end event

type dw_2 from w_mant_encab_deta_csd`dw_2 within w_maed_spro_cierredetencionlineas
integer x = 96
integer y = 108
integer width = 1801
integer height = 944
integer taborder = 10
boolean enabled = false
string dataobject = "dw_mant_spro_detencionlineaenca"
end type

event dw_2::itemchanged;String		ls_Columna, ls_Null
Date		ld_Fecha
Time		lt_Hora

SetNull(ls_Null)
ls_Columna	= dwo.Name

CHOOSE CASE ls_Columna
			
	CASE "line_codigo"
		IF iuo_LineaPacking.Existe(gstr_ParamPlanta.CodigoPlanta,Integer(data),True,Sqlca) THEN
			ExisteEncabezado(ls_Columna,data)
			istr_Mant.Argumento[2] = Data		
		ELSE
			This.SetItem(il_Fila, ls_Columna, Integer(ls_Null))
			RETURN 1
		END IF	
	CASE "espe_codigo"
		
		IF NOT iuo_especie.Existe(Integer(Data), True, SqlCa) THEN
			This.SetItem(il_Fila, ls_Columna, Integer(ls_Null))
			RETURN 1
		ELSE
			ExisteEncabezado(ls_Columna,data)
			istr_Mant.Argumento[3]	=	Data			
		END IF
			
	CASE "edla_fecpro"	
		ExisteEncabezado(ls_Columna,data)
		istr_Mant.Argumento[4] = Data
		
	CASE "rdla_turno"	
		ExisteEncabezado(ls_Columna,data)
		istr_Mant.Argumento[5] = Data
		
	CASE "edla_estado"		
		IF	validaminutos() THEN
			This.SetItem(il_Fila, "edla_estado",1)
			RETURN 1
		ELSE
			pb_grabar.Enabled = TRUE
		END IF	
END CHOOSE

HabilitaIngreso(ls_Columna)
end event

type pb_nuevo from w_mant_encab_deta_csd`pb_nuevo within w_maed_spro_cierredetencionlineas
integer x = 2277
integer y = 468
integer height = 124
integer taborder = 70
end type

type pb_eliminar from w_mant_encab_deta_csd`pb_eliminar within w_maed_spro_cierredetencionlineas
boolean visible = false
integer x = 2053
integer y = 396
integer taborder = 0
boolean enabled = true
end type

type pb_grabar from w_mant_encab_deta_csd`pb_grabar within w_maed_spro_cierredetencionlineas
integer x = 2277
integer y = 632
integer taborder = 50
end type

type pb_imprimir from w_mant_encab_deta_csd`pb_imprimir within w_maed_spro_cierredetencionlineas
integer x = 2249
integer y = 796
integer taborder = 60
end type

type pb_salir from w_mant_encab_deta_csd`pb_salir within w_maed_spro_cierredetencionlineas
integer x = 2277
integer taborder = 80
end type

type pb_ins_det from w_mant_encab_deta_csd`pb_ins_det within w_maed_spro_cierredetencionlineas
integer x = 2286
integer y = 1292
integer taborder = 20
end type

type pb_eli_det from w_mant_encab_deta_csd`pb_eli_det within w_maed_spro_cierredetencionlineas
integer x = 2286
integer y = 1460
integer taborder = 40
end type

type pb_buscar from w_mant_encab_deta_csd`pb_buscar within w_maed_spro_cierredetencionlineas
integer x = 2277
integer y = 256
integer taborder = 0
end type

