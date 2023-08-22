$PBExportHeader$w_maed_detencionlinea.srw
forward
global type w_maed_detencionlinea from w_mant_encab_deta_csd
end type
type tab_1 from tab within w_maed_detencionlinea
end type
type tabpage_1 from userobject within tab_1
end type
type dw_horas from uo_dw within tabpage_1
end type
type tabpage_1 from userobject within tab_1
dw_horas dw_horas
end type
type tabpage_2 from userobject within tab_1
end type
type dw_minutos from datawindow within tabpage_2
end type
type tabpage_2 from userobject within tab_1
dw_minutos dw_minutos
end type
type tab_1 from tab within w_maed_detencionlinea
tabpage_1 tabpage_1
tabpage_2 tabpage_2
end type
end forward

global type w_maed_detencionlinea from w_mant_encab_deta_csd
integer width = 2578
integer height = 2172
string title = "DETENCION LINEA"
string menuname = ""
event ue_validaregistro ( )
tab_1 tab_1
end type
global w_maed_detencionlinea w_maed_detencionlinea

type variables
Integer	ii_seleccion, il_filla
Date     idt_fecini, idt_horini

uo_especie				iuo_especie
uo_lineaPacking		iuo_lineaPacking

DataWindowChild		idwc_especie, idwc_planta,idwc_linea, idwc_area
DataWindow				dw_3, dw_4
end variables

forward prototypes
protected function boolean wf_actualiza_db (boolean borrando)
public subroutine habilita_encab (boolean habilita)
public function boolean duplicarea (string codigo)
public function boolean existeencabezado (string as_columna, string as_valor)
public subroutine habilitaingreso (string columna)
public function boolean duplicado (string as_columna, string as_valor)
end prototypes

event ue_validaregistro();Integer	li_cont,li_cont1,ll_fila
String	ls_mensaje,ls_colu[],ls_colu1[]
	
IF dw_3.GetItemStatus(il_Fila, 0, Primary!) = NewModified! THEN
	IF Isnull(dw_3.Object.rdla_turno[il_fila]) OR dw_3.Object.rdla_turno[il_fila] = 0 THEN
		li_cont ++
		ls_mensaje 			= ls_mensaje + "~nCódigo Turno"
		ls_colu[li_cont]	= "rdla_turno"
	END IF
		
	IF li_cont > 0 THEN
		MessageBox("Error de Consistencia", "Falta el ingreso de :" + ls_mensaje + ".", StopSign!, Ok!)
		dw_3.SetColumn(ls_colu[1])
		dw_3.SetFocus()
		Message.DoubleParm = -1
	END IF
END IF

IF dw_4.GetItemStatus(il_Fila, 0, Primary!) = NewModified! THEN
	IF Isnull(dw_4.Object.rdla_turno[il_fila]) OR dw_4.Object.rdla_turno[il_fila] = 0 THEN
		li_cont ++
		ls_mensaje 			= ls_mensaje + "~nCódigo Turno"
		ls_colu[li_cont]	= "rdla_turno"
	END IF
		
	IF li_cont > 0 THEN
		MessageBox("Error de Consistencia", "Falta el ingreso de :" + ls_mensaje + ".", StopSign!, Ok!)
		dw_4.SetColumn(ls_colu[1])
		dw_4.SetFocus()
		Message.DoubleParm = -1
	END IF
END IF

	
end event

protected function boolean wf_actualiza_db (boolean borrando);Boolean	lb_AutoCommit, lb_Retorno

IF Not dw_2.uf_check_required(0) THEN RETURN False

//IF Not dw_3.uf_validate(0) THEN RETURN False

lb_AutoCommit		=	sqlca.AutoCommit
sqlca.AutoCommit	=	False

IF Borrando THEN
	IF dw_3.Update(True, False) = 1 THEN
		IF dw_4.Update(True,False) =	1	THEN
			IF dw_2.Update(True, False) = 1 THEN
				Commit;
				
				IF sqlca.SQLCode <> 0 THEN
					F_ErrorBaseDatos(sqlca, This.Title)
				ELSE
					lb_Retorno	=	True
					
					dw_3.ResetUpdate()
					dw_4.ResetUpdate()
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
		F_ErrorBaseDatos(sqlca, This.Title)
		
		RollBack;
	END IF
ELSE
	IF dw_2.Update(True, False) = 1 THEN
		IF dw_4.Update(True,False) =	1	THEN
			IF dw_3.Update(True, False) = 1 THEN
				Commit;
				
				IF sqlca.SQLCode <> 0 THEN
					F_ErrorBaseDatos(sqlca, This.Title)
				ELSE
					lb_Retorno	=	True
					
					dw_3.ResetUpdate()
					dw_4.ResetUpdate()
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
	dw_2.Object.edla_estado.Protect				=	1
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
	dw_2.Object.edla_estado.Protect				=	1
	dw_2.Object.edla_fecpro.BackGround.Color	=	RGB(192,192,192)
	dw_2.Object.plde_codigo.BackGround.Color	=	RGB(192,192,192)
	dw_2.Object.line_codigo.BackGround.Color	=	RGB(192,192,192)
	dw_2.Object.espe_codigo.BackGround.Color	=	RGB(192,192,192)
	dw_2.Object.edla_fecpro.BackGround.Color	=	RGB(192,192,192)
	dw_2.Object.edla_estado.BackGround.Color	=	RGB(192,192,192)
END IF
end subroutine

public function boolean duplicarea (string codigo);Long		ll_fila


ll_fila	= dw_4.Find("area_codigo =" + codigo ,1, dw_1.RowCount())


IF ll_fila > 0 and ll_fila <> il_fila THEN
	MessageBox("Error","Código de area, ya fue Ingresado Anteriormente",Information!, Ok!)
	RETURN True
ELSE
	RETURN False
END IF
end function

public function boolean existeencabezado (string as_columna, string as_valor);Boolean	lb_Retorno
Integer	li_Planta, li_Linea, li_Especie, li_Cantidad, li_Turno
Date  	ldt_Fecha, ldt_FechaNula
String   ls_fecha, ls_hora

li_Planta		=	dw_2.Object.plde_codigo[1]
li_Linea			=	dw_2.Object.line_codigo[1]
li_Especie		=	dw_2.Object.espe_codigo[1]
ldt_fecha		=	dw_2.Object.edla_fecpro[1]


CHOOSE CASE as_Columna	
	CASE "line_codigo"
		li_Linea		=	Integer(as_valor)

	CASE "espe_codigo"
		li_Especie	=	Integer(as_valor)
		
	CASE "edla_fecpro"
		ldt_Fecha	=	Date(as_Valor)		
		
	CASE "rdla_turno"
		li_Turno		=	Integer(as_valor)
		
END CHOOSE

SELECT	Count(*)
	INTO	:li_Cantidad
	FROM	dba.spro_detencionlineaenca
	WHERE	plde_codigo	=	:li_Planta
	AND   line_codigo	=	:li_Linea
	AND	espe_codigo	=	:li_Especie
	AND	edla_fecpro	=	:ldt_Fecha 
	AND	rdla_turno	=	:li_Turno
	AND 	edla_estado	=	1;

IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de Tabla Spro_detencionlineaenca ")
	
	RETURN TRUE
ELSEIF sqlca.SQLCode <> 100 and li_cantidad>0 THEN
	istr_mant.argumento[1]	=	String(li_Planta)
	istr_mant.argumento[2]	=	String(li_Linea)
	istr_mant.argumento[3]	=	String(li_Especie)
	istr_mant.argumento[4]	=	String(ldt_Fecha)
	istr_mant.argumento[6]	=	String(li_Turno)

	This.TriggerEvent("ue_recuperadatos")
	
	lb_Retorno	=	True
ELSE
	lb_Retorno	=	False
END IF

RETURN lb_Retorno
end function

public subroutine habilitaingreso (string columna);Boolean	lb_Estado = True
Date   	ldt_Hora

IF IsNull(dw_2.Object.line_codigo[il_fila]) OR dw_2.Object.line_codigo[il_fila] = 0 THEN
	lb_Estado	=	False
END IF


pb_ins_det.Enabled	=	lb_Estado
end subroutine

public function boolean duplicado (string as_columna, string as_valor);Long		ll_fila
String 	ls_Fecha,ls_Hora

ls_Fecha	=  Mid(String(dw_3.Object.fecha_ini[il_fila]),1,10)
ls_Hora	=  Mid(String(dw_3.Object.hdla_horini[il_fila]),12,8)


CHOOSE CASE as_Columna
		
	CASE "fecha_ini"
		ls_Fecha	=	String(Date(Mid(as_Valor,1,10)),'dd-mm-yyyy')
		
	CASE "hdla_horini"
		ls_Hora	=	Mid(as_Valor,12,8)
		
END CHOOSE

ll_fila	= dw_3.Find("String(Date(fecha_ini),'dd-mm-yyyy') ='"+ls_fecha+ &
						" AND String(Time(hdla_horini,'hh:mm:ss') ='"+ls_hora+"'",1, dw_3.RowCount())


IF ll_fila > 0 and ll_fila <> il_fila THEN
	MessageBox("Error","Hora y Fecha Inicio, ya fue Ingresado Anteriormente",Information!, Ok!)
	RETURN True
ELSE
	RETURN False
END IF

end function

event ue_seleccion();OpenWithParm(w_busc_detencionlinea, istr_busq)

istr_busq	= Message.PowerObjectParm

IF istr_busq.argum[2] <> "" THEN
	if  istr_busq.argum[1] = String(gstr_ParamPlanta.CodigoPlanta) then
		istr_mant.argumento[1] = String(gstr_ParamPlanta.CodigoPlanta)
		istr_mant.argumento[2] = istr_busq.argum[2]
		istr_mant.argumento[3] = istr_busq.argum[3]
		istr_mant.argumento[4] = istr_busq.argum[4]
		istr_mant.argumento[6] = istr_busq.argum[5]
	
		TriggerEvent("ue_recuperadatos")
	else
		messagebox("No se pueden mostrar los datos","No es posible mostrar datos de otra planta ~r Seleccione un registro correspondiente" )
	end if
ELSE
	pb_buscar.SetFocus()
END IF
end event

event ue_recuperadatos();call super::ue_recuperadatos;Long		ll_fila_d, ll_fila_e, respuesta,ll_fila_f
Date  	ld_Fecha
Time		lt_Hora

ld_Fecha		=	Date(Mid(istr_mant.Argumento[4], 1, 10))
lt_Hora		=	Time(Mid(istr_mant.Argumento[4], 12))

DO
	dw_2.SetRedraw(False)
	
	ll_fila_e	=	dw_2.Retrieve(Integer(istr_mant.Argumento[1]), &
										Integer(istr_mant.Argumento[2]), &
										Integer(istr_mant.Argumento[3]), &
										ld_Fecha, &
										Integer(istr_mant.Argumento[6]))
	
	IF ll_fila_e = -1 THEN
		respuesta = MessageBox(	"Error en Base de Datos", "No es posible conectar la Base de Datos.", &
										Information!, RetryCancel!)
	ELSE
		DO
			ll_fila_d	=	dw_3.Retrieve(Integer(istr_mant.Argumento[1]), &
										Integer(istr_mant.Argumento[2]), &
										Integer(istr_mant.Argumento[3]), &
										ld_fecha, &
										Integer(istr_mant.Argumento[6]))
			IF ll_fila_d = -1 THEN
				respuesta = MessageBox(	"Error en Base de Datos", "No es posible conectar la Base de Datos.", &
												Information!, RetryCancel!)
			ELSE	
				DO 
					ll_fila_f	=	dw_4.Retrieve(Integer(istr_mant.Argumento[1]), &
										Integer(istr_mant.Argumento[2]), &
										Integer(istr_mant.Argumento[3]),&
										ld_fecha, &
										Integer(istr_mant.Argumento[6]))
					IF ll_fila_f = -1 THEN
						respuesta = MessageBox(	"Error en Base de Datos", "No es posible conectar la Base de Datos.", &
														Information!, RetryCancel!)		
					ELSE
						pb_eliminar.Enabled	=	True
						pb_grabar.Enabled		=	True
						pb_ins_det.Enabled	=	True
						
						IF ll_fila_d > 0 OR ll_fila_e > 0 OR ll_fila_f >0 THEN
							pb_eli_det.Enabled	=	True
							pb_imprimir.Enabled	=	True
							dw_3.SetRow(1)
							dw_4.SetRow(1)
							pb_ins_det.SetFocus()							
							Habilita_Encab(False)
						ELSE
							pb_ins_det.SetFocus()
						END IF
					END IF				
				LOOP WHILE respuesta = 1
				IF respuesta = 2 THEN Close(This)
			END IF
		LOOP WHILE respuesta = 1
		IF respuesta = 2 THEN Close(This)
	END IF	
	dw_2.SetRedraw(True)
LOOP WHILE respuesta = 1

IF respuesta = 2 THEN Close(This)


end event

event ue_nuevo_detalle();Integer	li_tabpage
String ls_hora

li_tabpage			=	tab_1.SelectedTab

CHOOSE CASE li_tabpage
	CASE 1		
		istr_mant.dw	=	dw_3
		IF ii_seleccion	=	0	THEN	
			IF il_fila > 0 THEN
				pb_eliminar.Enabled	= True
				pb_grabar.Enabled		= True
				pb_eli_det.Enabled	= True
			END IF
			
			il_fila = dw_3.InsertRow(0)
			
			dw_3.ScrollToRow(il_fila)
			dw_3.SetRow(il_fila)
			dw_3.SetFocus()
			dw_3.Object.rdla_turno[il_fila]	=	dw_2.Object.rdla_turno[1]
//			dw_3.Object.fecha_ini[il_fila]	=	DATE(Today())
//			ls_hora                       	=  MID(String(Now()),1,5)
//			ls_hora									=	ls_hora+':00'
//			dw_3.Object.fecha_ini[il_fila]	=	Date(Today())

			dw_3.SetColumn("rdla_turno")
		ELSE
			IF il_fila > 0 THEN
				pb_eliminar.Enabled	= True
				pb_grabar.Enabled		= True
				pb_eli_det.Enabled	= True
			END IF
		END IF
		
	CASE 2
		istr_mant.dw	=	dw_4
		IF ii_seleccion	=	0	THEN	
			IF il_fila > 0 THEN
				pb_eliminar.Enabled	= True
				pb_grabar.Enabled		= True
				pb_eli_det.Enabled	= True
			END IF
			
			il_fila = dw_4.InsertRow(0)
			
			dw_4.Object.rdla_turno[il_fila]	=	dw_2.Object.rdla_turno[1]
			
			dw_4.ScrollToRow(il_fila)
			dw_4.SetRow(il_fila)
			dw_4.SetFocus()
			
			dw_4.SetColumn("area_codigo")
		ELSE
			IF il_fila > 0 THEN
				pb_eliminar.Enabled	= True
				pb_grabar.Enabled		= True
				pb_eli_det.Enabled	= True
			END IF
		END IF

		
END CHOOSE

end event

event ue_imprimir();//SetPointer(HourGlass!)
//
//Long		fila
//str_info	lstr_info
//
//lstr_info.titulo	= "DETENCION LINEAS"
//lstr_info.copias	= 1
//
//OpenWithParm(vinf,lstr_info)
//
//vinf.dw_1.DataObject = "dw_info_detencionlinea"
//
//vinf.dw_1.SetTransObject(sqlca)
//
//fila = vinf.dw_1.Retrieve(Integer(istr_mant.argumento[1]),0, &
//								  Integer(istr_mant.argumento[2]),Integer(istr_mant.argumento[3]),&
//								  Date(istr_mant.argumento[4])
//
//IF fila = -1 THEN
//	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
//					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
//ELSEIF fila = 0 THEN
//	MessageBox( "No Existe información", "No existe información para este informe.", &
//					StopSign!, Ok!)
//ELSE
//	F_Membrete(vinf.dw_1)
//	vinf.dw_1.Modify('DataWindow.Print.Preview = Yes')
//	vinf.dw_1.Modify('DataWindow.Print.Preview.Zoom = 75')
//
//	vinf.Visible	= True
//	vinf.Enabled	= True
//END IF
//
//SetPointer(Arrow!)
end event

event ue_borra_detalle();IF istr_Mant.Solo_Consulta THEN RETURN


Integer	li_tabpage

li_tabpage			=	tab_1.SelectedTab

CHOOSE CASE li_tabpage
	CASE 1		
		IF dw_3.rowcount() < 1 THEN RETURN
		
			SetPointer(HourGlass!)
		 
			ib_borrar = True
			w_main.SetMicroHelp("Validando la eliminación de detalle...")
		
			Message.DoubleParm = 0
		
			This.TriggerEvent ("ue_validaborrar_detalle")
		
			IF Message.DoubleParm = -1 THEN RETURN
		
			IF MessageBox("Eliminación de Registro", "Está seguro de Eliminar este Registro", Question!, YesNo!) = 1 THEN
				IF dw_3.DeleteRow(0) = 1 THEN
					ib_borrar = False
					w_main.SetMicroHelp("Borrando Registro...")
					SetPointer(Arrow!)
				ELSE
					ib_borrar = False
					MessageBox(This.Title,"No se puede borrar actual registro.")
				END IF
		
				IF dw_3.RowCount() = 0 THEN
					pb_eliminar.Enabled = False
				ELSE
					il_fila = dw_3.GetRow()
				END IF
			END IF

		
	CASE 2
		IF dw_4.rowcount() < 1 THEN RETURN

       SetPointer(HourGlass!)
 
       ib_borrar = True
       w_main.SetMicroHelp("Validando la eliminación de detalle...")

       Message.DoubleParm = 0

       This.TriggerEvent ("ue_validaborrar_detalle")
      
       IF Message.DoubleParm = -1 THEN RETURN
      
      IF MessageBox("Eliminación de Registro", "Está seguro de Eliminar este Registro", Question!, YesNo!) = 1 THEN
         IF dw_4.DeleteRow(0) = 1 THEN
	         ib_borrar = False
		      w_main.SetMicroHelp("Borrando Registro...")
		      SetPointer(Arrow!)
         ELSE
	      	ib_borrar = False
	   	   MessageBox(This.Title,"No se puede borrar actual registro.")
         END IF

         IF dw_4.RowCount() = 0 THEN
 	         pb_eliminar.Enabled = False
         ELSE
		      il_fila = dw_4.GetRow()
         END IF
     END IF

		
END CHOOSE


//IF ii_seleccion = 0 THEN
//   IF dw_3.rowcount() < 1 THEN RETURN
//
//   SetPointer(HourGlass!)
// 
//   ib_borrar = True
//   w_main.SetMicroHelp("Validando la eliminación de detalle...")
//
//   Message.DoubleParm = 0
//
//   This.TriggerEvent ("ue_validaborrar_detalle")
//
//   IF Message.DoubleParm = -1 THEN RETURN
//
//   IF MessageBox("Eliminación de Registro", "Está seguro de Eliminar este Registro", Question!, YesNo!) = 1 THEN
//	   IF dw_3.DeleteRow(0) = 1 THEN
//	   	ib_borrar = False
//	   	w_main.SetMicroHelp("Borrando Registro...")
//		   SetPointer(Arrow!)
//	   ELSE
//		   ib_borrar = False
//		   MessageBox(This.Title,"No se puede borrar actual registro.")
//	   END IF
//
//      IF dw_3.RowCount() = 0 THEN
//		   pb_eliminar.Enabled = False
//	   ELSE
//		   il_fila = dw_3.GetRow()
//	   END IF
//   END IF
//ELSE 
//     IF dw_4.rowcount() < 1 THEN RETURN
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
//END IF
end event

on w_maed_detencionlinea.create
int iCurrent
call super::create
this.tab_1=create tab_1
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.tab_1
end on

on w_maed_detencionlinea.destroy
call super::destroy
destroy(this.tab_1)
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
istr_mant.argumento[5]	=	Area
istr_mant.argumento[6]	=	Turno
*/

x				= 0
y				= 0
This.Height	= 2020
im_menu		= m_principal

This.Icon									=	Gstr_apl.Icono
This.ParentWindow().ToolBarVisible	=	True
im_menu.Item[1].Item[6].Enabled		=	True
im_menu.Item[7].Visible					=	True

dw_3	=	tab_1.tabpage_1.dw_horas
dw_4	=	tab_1.tabpage_2.dw_minutos

dw_1.SetTransObject(sqlca)
dw_2.SetTransObject(sqlca)
dw_3.SetTransObject(sqlca)
dw_4.SetTransObject(sqlca)
dw_1.Modify("datawindow.message.title='Error '+ is_titulo")
dw_1.SetRowFocusIndicator(Hand!)
dw_1.Modify("DataWindow.Footer.Height = 110")

pb_nuevo.PostEvent(Clicked!)

iuo_especie				=	Create uo_especie
iuo_lineaPacking		=	Create uo_lineaPacking

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

//Area
dw_4.GetChild("area_codigo", idwc_area)
idwc_area.SetTransObject(sqlca)
IF idwc_area.Retrieve() = 0 THEN
	MessageBox("Atención","Falta Registrar Areas Responsables")
ELSE
	idwc_area.InsertRow(0)
END IF

istr_mant.argumento[1]	=	String(gstr_ParamPlanta.CodigoPlanta)
end event

event ue_antesguardar();Long		ll_Fila
Integer	li_min, li_Min2, li_contador
String   ls_fechaini, ls_horaini,ls_fechater, ls_horater, ls_mensaje
String   ls_Null

dw_3.accepttext()
dw_4.accepttext()

Setnull(ls_Null)

FOR il_fila = 1 TO dw_3.RowCount()
	
	IF dw_3.GetItemStatus(il_fila, 0, Primary!) = New! THEN
		
		dw_3.DeleteRow(il_fila)
		
	ELSEIF dw_3.GetItemStatus(il_fila, 0, Primary!) = NewModified! THEN
		
		TriggerEvent("ue_validaregistro")				

		dw_3.SetItem(il_fila, "plde_codigo", dw_2.Object.plde_codigo[1])
		dw_3.SetItem(il_fila, "line_codigo", dw_2.Object.line_codigo[1])
		dw_3.SetItem(il_fila, "espe_codigo", dw_2.Object.espe_codigo[1])		
		dw_3.SetItem(il_fila, "edla_fecpro", dw_2.Object.edla_fecpro[1])
		
		/* Compone Fecha de Hora Inicio */
		ls_fechaini	=  Mid(String(dw_3.Object.fecha_ini[il_fila]),1,10)
		ls_horaini	=  Mid(String(dw_3.Object.hdla_horini[il_fila]),1,5)
		ls_horaini  =  ls_horaini+':00'
		dw_3.Object.hdla_horini[il_fila]	=	Time(ls_Horaini)
		
		/* Compone Fecha de Hora Termino */
		ls_fechater	=  Mid(String(dw_3.Object.fecha_ter[il_fila]),1,10)
		ls_horater	=  Mid(String(dw_3.Object.hdla_horter[il_fila]),1,5)
		ls_horater  =  ls_horater+':00'
		dw_3.Object.hdla_horter[il_fila]	=	Time(ls_Horater)
		
	END IF

 	  IF IsNull(ls_fechaini) THEN
		  li_contador ++
		  ls_mensaje  = ls_mensaje + ("~nFecha Inicio")
	  END IF

     IF IsNull(ls_horaini) THEN 
		  li_contador ++
		  ls_mensaje  = ls_mensaje + ("~nHora de Inicio")
	  END IF

	  IF IsNull(ls_fechater) THEN
		  li_contador ++
		  ls_mensaje  = ls_mensaje + ("~nFecha de Termino")
	  END IF

	  IF IsNull(ls_horater) THEN
		  li_contador ++
		  ls_mensaje  = ls_mensaje + ("~nHora de Termino")
	  END IF
	  
	  IF li_Contador > 0 THEN
		  MessageBox("Detención de Lineas", "Falta el Ingreso de: " + ls_Mensaje + &
			 			" ", StopSign!, Ok!)
		  Message.DoubleParm = -1
	  END IF

NEXT


FOR il_fila = 1 TO dw_4.RowCount()
	
	IF dw_4.GetItemStatus(il_fila, 0, Primary!) = New! THEN
		
		dw_4.DeleteRow(il_fila)
		
	ELSEIF dw_4.GetItemStatus(il_fila, 0, Primary!) = NewModified! THEN
		
		TriggerEvent("ue_validaregistro")				

		dw_4.SetItem(il_fila, "plde_codigo", dw_2.Object.plde_codigo[1])
		dw_4.SetItem(il_fila, "line_codigo", dw_2.Object.line_codigo[1])
		dw_4.SetItem(il_fila, "espe_codigo", dw_2.Object.espe_codigo[1])		
		dw_4.SetItem(il_fila, "edla_fecpro", dw_2.Object.edla_fecpro[1])
	END IF
	
NEXT

IF dw_3.RowCount() > 0 AND dw_4.RowCount() > 0 THEN 
	li_min  = Integer(dw_3.Object.total_minutos[1])
	li_Min2 = Integer(dw_4.Object.min_total[1])
END IF

IF li_min <> li_Min2 THEN
	MessageBox("Error de Consistencia","Los Minutos de Detención, Deben"+&
					"~r~r Ser Igual a Los de Areas Involucradas",Exclamation!)
	Message.DoubleParm = -1				
	RETURN
END IF	

FOR ll_Fila = 1 TO dw_4.RowCount()
	dw_4.Object.enlace[ll_Fila]	=	1
NEXT
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

pb_eli_det.Enabled		=	False
pb_ins_det.Enabled		=	False
pb_grabar.Enabled			=	False
pb_eliminar.Enabled		=	False
pb_imprimir.Enabled		=	False
dw_2.Enabled				=	True

dw_2.SetRedraw(False)
dw_2.Reset()
dw_2.InsertRow(0)

dw_2.Object.edla_fecpro[1]		=	Date(Today())
istr_Mant.Argumento[4]			=  String(Date(Today()))

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

event ue_borrar();IF istr_Mant.Solo_Consulta THEN RETURN

IF dw_3.RowCount() < 1 OR dw_2.Rowcount() < 1 THEN RETURN

SetPointer(HourGlass!)

ib_borrar = True
w_main.SetMicroHelp("Validando la eliminación...")

Message.DoubleParm = 0

This.TriggerEvent ("ue_validaborrar")

IF Message.DoubleParm = -1 THEN RETURN

IF dw_3.RowCount() > 0 THEN dw_3.RowsMove(1, dw_3.RowCount(), Primary!, dw_3, 1, Delete!)

IF dw_1.RowCount() > 0 THEN dw_1.RowsMove(1, dw_1.RowCount(), Primary!, dw_1, 1, Delete!)

IF dw_2.DeleteRow(0) = 1 THEN
	ib_borrar	=	False
	
	w_main.SetMicroHelp("Borrando Registro...")
	
	IF wf_actualiza_db(True) THEN
		w_main.SetMicroHelp("Registro Borrado...")
		This.TriggerEvent("ue_nuevo")
		SetPointer(Arrow!)
	ELSE
		w_main.SetMicroHelp("Registro no Borrado...")
	END IF
END IF
end event

event resize;Integer	maximo, li_posic_x, li_posic_y, li_visible = 0

maximo	= tab_1.width

IF dw_2.width > maximo THEN maximo = dw_2.width

dw_2.x					=	37 + Round((maximo - dw_2.width) / 2, 0)
dw_2.y					=	37

tab_1.x					=	37 + Round((maximo - tab_1.width) / 2, 0)
tab_1.y					=	64 + dw_2.Height
tab_1.height			=	This.WorkSpaceHeight() - dw_1.y - 41

tab_1.tabpage_1.dw_horas.x				=	46
tab_1.tabpage_1.dw_horas.y				=	40
tab_1.tabpage_1.dw_horas.Height		=	tab_1.Height - 192
tab_1.tabpage_1.dw_horas.Width		=	tab_1.Width - 119

tab_1.tabpage_2.dw_minutos.x			=	46
tab_1.tabpage_2.dw_minutos.y			=	40
tab_1.tabpage_2.dw_minutos.Height	=	tab_1.Height - 192
tab_1.tabpage_2.dw_minutos.Width		=	tab_1.Width - 119

//gb_1.x 					= This.WorkSpaceWidth() - 310
//gb_1.y 					= 5
//gb_1.Width				= 275

li_posic_x				= This.WorkSpaceWidth() - 250
//li_posic_y				= gb_1.y + 88

IF pb_buscar.Visible THEN
	pb_buscar.x				= li_posic_x
	pb_buscar.y				= li_posic_y
	pb_buscar.width		= 156
	pb_buscar.height		= 133
	li_visible ++
	li_posic_y += 180
END IF

IF pb_nuevo.Visible THEN
	pb_nuevo.x				= li_posic_x
	pb_nuevo.y				= li_posic_y
	pb_nuevo.width			= 156
	pb_nuevo.height		= 133
	li_visible ++
	li_posic_y += 180
END IF

IF	pb_eliminar.Visible THEN
	pb_eliminar.x			= li_posic_x
	pb_eliminar.y			= li_posic_y
	pb_eliminar.width		= 156
	pb_eliminar.height	= 133
	li_visible ++
	li_posic_y += 180
END IF

IF pb_grabar.Visible THEN
	pb_grabar.x				= li_posic_x
	pb_grabar.y				= li_posic_y
	pb_grabar.width		= 156
	pb_grabar.height		= 133
	li_visible ++
	li_posic_y += 180
END IF

IF pb_imprimir.Visible THEN
	pb_imprimir.x			= li_posic_x
	pb_imprimir.y			= li_posic_y
	pb_imprimir.width		= 156
	pb_imprimir.height	= 133
	li_visible ++
	li_posic_y += 180
END IF

IF pb_salir.Visible THEN
	pb_salir.x				= li_posic_x
	pb_salir.y				= li_posic_y
	pb_salir.width			= 156
	pb_salir.height		= 133
	li_visible ++
	li_posic_y += 180
END IF

//gb_1.height				= 180 * li_visible + 97 /*  (Según Botones Visibles)  */
//gb_2.x 					= gb_1.x
//gb_2.y 					= 1293
//gb_2.width				= 275
//gb_2.height				= 180 * 2 + 97 /*  (2 Botones)  */

pb_ins_det.x			= li_posic_x
//pb_ins_det.y			= gb_2.y + 93
pb_ins_det.width		= 156
pb_ins_det.height		= 133

pb_eli_det.x			= li_posic_x
pb_eli_det.y			= pb_ins_det.y + 180
pb_eli_det.width		= 156
pb_eli_det.height		= 133
end event

type dw_1 from w_mant_encab_deta_csd`dw_1 within w_maed_detencionlinea
boolean visible = false
integer y = 992
integer width = 2085
integer height = 816
integer taborder = 30
boolean enabled = false
boolean titlebar = false
string title = "Hora"
string dataobject = "dw_mues_spro_detencionlineahora"
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

type dw_2 from w_mant_encab_deta_csd`dw_2 within w_maed_detencionlinea
integer x = 96
integer y = 40
integer width = 1792
integer height = 944
integer taborder = 10
boolean enabled = false
string dataobject = "dw_mant_spro_detencionlineaenca"
end type

event dw_2::itemchanged;String		ls_Columna, ls_Null
Date			ld_Fecha
Time		 	lt_Hora

SetNull(ls_Null)
ls_Columna	= dwo.Name

CHOOSE CASE ls_Columna
			
	CASE "line_codigo"
		
		IF iuo_LineaPacking.Existe(gstr_ParamPlanta.CodigoPlanta,Integer(data),True,Sqlca) THEN
			istr_Mant.Argumento[2] = Data		
			ExisteEncabezado(ls_Columna,data)
		ELSE
			This.SetItem(il_Fila, ls_Columna, Integer(ls_Null))
			RETURN 1
		END IF	
		
	CASE "espe_codigo"
		
		IF iuo_especie.Existe(Integer(Data), True, SqlCa) THEN
			istr_Mant.Argumento[3]	=	Data
			ExisteEncabezado(ls_Columna,data)
		ELSE
			This.SetItem(il_Fila, ls_Columna, Integer(ls_Null))
			RETURN 1		
		END IF
			
	CASE "edla_fecpro"	
		
		istr_Mant.Argumento[4] = Data
		ExisteEncabezado(ls_Columna,data)
		
	CASE "rdla_turno"	
		
		istr_Mant.Argumento[6] = Data
		ExisteEncabezado(ls_Columna,data)
		
END CHOOSE

HabilitaIngreso(ls_Columna)
end event

type pb_nuevo from w_mant_encab_deta_csd`pb_nuevo within w_maed_detencionlinea
integer x = 2245
integer y = 468
integer height = 124
integer taborder = 70
end type

event pb_nuevo::clicked;call super::clicked;dw_3.Reset()
dw_4.Reset()
end event

type pb_eliminar from w_mant_encab_deta_csd`pb_eliminar within w_maed_detencionlinea
integer x = 2245
integer y = 396
integer taborder = 0
end type

type pb_grabar from w_mant_encab_deta_csd`pb_grabar within w_maed_detencionlinea
integer x = 2245
integer y = 632
integer taborder = 50
end type

type pb_imprimir from w_mant_encab_deta_csd`pb_imprimir within w_maed_detencionlinea
boolean visible = false
integer x = 2231
integer y = 820
integer taborder = 60
end type

type pb_salir from w_mant_encab_deta_csd`pb_salir within w_maed_detencionlinea
integer x = 2245
integer taborder = 80
end type

type pb_ins_det from w_mant_encab_deta_csd`pb_ins_det within w_maed_detencionlinea
integer x = 2254
integer y = 1292
integer taborder = 20
end type

type pb_eli_det from w_mant_encab_deta_csd`pb_eli_det within w_maed_detencionlinea
integer x = 2254
integer y = 1460
integer taborder = 40
end type

type pb_buscar from w_mant_encab_deta_csd`pb_buscar within w_maed_detencionlinea
integer x = 2245
integer y = 256
integer taborder = 0
end type

type tab_1 from tab within w_maed_detencionlinea
event create ( )
event destroy ( )
integer x = 87
integer y = 992
integer width = 2002
integer height = 816
integer taborder = 40
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
boolean raggedright = true
boolean focusonbuttondown = true
integer selectedtab = 1
tabpage_1 tabpage_1
tabpage_2 tabpage_2
end type

on tab_1.create
this.tabpage_1=create tabpage_1
this.tabpage_2=create tabpage_2
this.Control[]={this.tabpage_1,&
this.tabpage_2}
end on

on tab_1.destroy
destroy(this.tabpage_1)
destroy(this.tabpage_2)
end on

type tabpage_1 from userobject within tab_1
event create ( )
event destroy ( )
integer x = 18
integer y = 112
integer width = 1966
integer height = 688
long backcolor = 12632256
string text = "Tiempo de Detención"
long tabtextcolor = 33554432
long tabbackcolor = 12632256
long picturemaskcolor = 536870912
dw_horas dw_horas
end type

on tabpage_1.create
this.dw_horas=create dw_horas
this.Control[]={this.dw_horas}
end on

on tabpage_1.destroy
destroy(this.dw_horas)
end on

type dw_horas from uo_dw within tabpage_1
integer x = 46
integer y = 40
integer width = 1815
integer height = 624
integer taborder = 50
string title = "Tiempo de Detención"
string dataobject = "dw_mues_spro_detencionlineahora"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event rowfocuschanged;call super::rowfocuschanged;ib_datos_ok = True

IF This.RowCount() < 1 OR CurrentRow = 0 OR ib_borrar THEN
	ib_datos_ok = False
ELSE
	il_fila = CurrentRow
END IF
end event

event losefocus;call super::losefocus;Accepttext()
end event

event clicked;IF Row > 0 THEN
	il_fila = Row
END IF

RETURN 0
end event

event itemerror;call super::itemerror;RETURN 1
end event

event itemchanged;call super::itemchanged;String	ls_null, ls_Columna
String   ls_horaini, ls_horater,ls_fechaini,ls_fechater
Time 		lt_horaini,lt_horater

SetNull(ls_null)

ls_Columna	=	dwo.Name
//dw_3	=	tab_1.tabpage_1.dw_horas
//dw_3.accepttext()
CHOOSE CASE ls_Columna
		
	CASE "fecha_ini"
		
		IF Date(Mid(Data, 1, 10)) < Date(dw_2.Object.edla_fecpro[1]) THEN
			MessageBox("Error", "Fecha de Inicio no debe ser Anterior a Fecha Proceso ." + &
							"~r~rIngrese otra Fecha de Termino.")			
			This.SetItem(il_fila, "fecha_ini", Date(today()))			
			RETURN 1
		END IF	
//		ELSEIF Duplicado(ls_Columna,data) THEN
//			This.SetItem(il_fila, "fecha_ini",Date(ls_null))
//			RETURN 1
//		END IF
		
	CASE "hdla_horini"
		IF dw_3.RowCount() >1 THEN
			ls_fechaini	=  Mid(String(dw_3.Object.fecha_ini[il_fila]),1,10)
			ls_horaini	=  Mid(String(Data),1,5)
			ls_horaini  =  ls_horaini+':00'
			
			ls_fechater	=  Mid(String(dw_3.Object.fecha_ter[il_fila -1]),1,10)
			ls_horater	=  String(dw_3.Object.hdla_horter[il_fila - 1])
			ls_horater  =  ls_horater+':00'
		
			lt_horaini  =  Time(ls_Horaini)
			lt_horater  =  Time(ls_Horater)
			
			IF lt_horaini <= lt_horater THEN
				MessageBox("Error", "Hora Inicio No debe ser Anterior ó Igual a  Hora Termino." + &
								"~r~rIngrese otra Hora de Termino.")			
				This.SetItem(il_fila, "hdla_horini", Time(ls_Null))			
				RETURN 1
			END IF	
				
//			ELSEIF Duplicado(ls_Columna, data) THEN
//				This.SetItem(il_fila, "hdla_horini",Date(ls_null))
//				RETURN 1
//			END IF	
		END IF
		
	CASE "fecha_ter"
		IF Date(Mid(Data, 1, 10)) < Date(dw_3.Object.fecha_ini[il_fila]) THEN
			MessageBox("Error", "Fecha de Termino no debe ser Anterior a Inicio." + &
							"~r~rIngrese otra Fecha de Termino.")			
			This.SetItem(il_fila, "fecha_ter", Date(ls_Null))			
			RETURN 1
		END IF
			
	CASE "hdla_horter"
		ls_fechaini	=  Mid(String(dw_3.Object.fecha_ini[il_fila]),1,10)
		ls_horaini	=  String(dw_3.Object.hdla_horini[il_fila])
		ls_horaini  =  ls_horaini+':00'
				
		ls_fechater	=  Mid(String(dw_3.Object.fecha_ter[il_fila]),1,10)
		ls_horater	=  Mid(String(Data),1,5)
		ls_horater  =  ls_horater+':00'
	
		lt_horaini = Time(ls_Horaini)
		lt_horater = Time(ls_Horater)
		
		IF lt_horaini >= lt_horater THEN
			MessageBox("Error", "Hora Termino No debe ser Anterior ó Igual a  Hora Inicio." + &
							"~r~rIngrese otra Hora de Termino.")			
			This.SetItem(il_fila, "hdla_horter", Time(ls_Null))			
			RETURN 1
		END IF	
			
END CHOOSE
end event

type tabpage_2 from userobject within tab_1
event create ( )
event destroy ( )
integer x = 18
integer y = 112
integer width = 1966
integer height = 688
long backcolor = 12632256
string text = "Areas Involucradas"
long tabtextcolor = 33554432
long tabbackcolor = 12632256
long picturemaskcolor = 536870912
dw_minutos dw_minutos
end type

on tabpage_2.create
this.dw_minutos=create dw_minutos
this.Control[]={this.dw_minutos}
end on

on tabpage_2.destroy
destroy(this.dw_minutos)
end on

type dw_minutos from datawindow within tabpage_2
integer x = 46
integer y = 48
integer width = 1870
integer height = 604
integer taborder = 50
boolean bringtotop = true
string title = "none"
string dataobject = "dw_mues_detencionlinearea"
boolean vscrollbar = true
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event rowfocuschanged;ib_datos_ok = True

IF This.RowCount() < 1 OR CurrentRow = 0 OR ib_borrar THEN
	ib_datos_ok = False
ELSE
	il_fila = CurrentRow
END IF
end event

event losefocus;Accepttext()
end event

event clicked;IF Row > 0 THEN
	il_fila = Row
END IF

RETURN 0
end event

event itemerror;RETURN 1
end event

event itemchanged;String	ls_null

SetNull(ls_null)

CHOOSE CASE GetColumnName()

	CASE "area_codigo"			
		IF Duplicarea(data) THEN
			This.SetItem(il_fila, "area_codigo",Long(ls_null))
			RETURN 1
		ELSE
			istr_Mant.Argumento[5]	=	Data
		END IF
		
	CASE "rdla_minuto"
		IF Integer(data) < 0 OR Integer(data) > 999 THEN
			Messagebox("Error de Consistencia","Cantidad de minutos supera lo permitido")
			This.SetItem(row, "rdla_minuto", Integer(ls_Null))
			RETURN 1
		END IF
		
END CHOOSE


end event

event buttonclicked;str_busqueda	lstr_busq

IF dwo.Name = 'b_enlace' THEN
	
	IF row = 0 THEN RETURN
	
	lstr_busq.Argum[1]	=	String(This.Object.area_codigo[row])
	lstr_busq.Argum[2]	=	istr_mant.Argumento[1]
	lstr_busq.Argum[3]	=	istr_mant.Argumento[3]
	lstr_busq.Argum[4]	=	istr_mant.Argumento[2]
	lstr_busq.Argum[5]	=	istr_mant.Argumento[4]
	lstr_busq.Argum[6]	=	istr_mant.Argumento[6]
	
	OpenWithParm(w_mant_mues_spro_detencionlineadeta, lstr_busq)
	
END IF
end event

