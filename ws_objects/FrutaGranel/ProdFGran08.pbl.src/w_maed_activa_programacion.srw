$PBExportHeader$w_maed_activa_programacion.srw
$PBExportComments$Ventana de mantención de Orden de Proceso de Fruta Granel.
forward
global type w_maed_activa_programacion from w_mant_encab_deta
end type
type dw_3 from datawindow within w_maed_activa_programacion
end type
type dw_unitech from uo_dw within w_maed_activa_programacion
end type
end forward

global type w_maed_activa_programacion from w_mant_encab_deta
integer width = 2597
integer height = 2048
string title = "Programaciones de Proceso"
string menuname = ""
dw_3 dw_3
dw_unitech dw_unitech
end type
global w_maed_activa_programacion w_maed_activa_programacion

type prototypes

end prototypes

type variables
uo_especie 			 					iuo_especie
uo_variedades 		 					iuo_variedad
uo_grupoespecie    					iuo_grupo
uo_subgrupoespecie 					iuo_subgrupo
uo_spro_ordenproceso 				iuo_ordenproceso
uo_productores							iuo_productores
uo_tratamientofrio						iuo_tratamientofrio
uo_periodofrio							iuo_periodofrio
uo_lineapacking						iuo_lineapacking
uo_lotesfrutagranel					iuo_lotesfrutagranel
uo_etiquetas							iuo_etiquetas

Boolean									ib_Modifica, ib_AutoCommit, lb_Actualiza = False, ib_conexionodbc
DataWindowChild						idwc_especie, idwc_variedad, idwc_planta, &
											idwc_categoria, idwc_grupo, idwc_subgrupo, idwc_embalaje, &
											idwc_etiqueta, idwc_recibidor, idwc_calibre, idwc_camara, &
											idwc_envase, idwc_calidad, idwc_cliente

DataWindowChild						idwc_productor, idwc_periodofrio, idwc_tratamiento, &
											idwc_lineapacking, idwc_linea

w_mant_deta_ordenproceso			iw_mantencion_1
w_mant_deta_programa_proc_cal		iw_consulta_1

Long										il_FilaDetProg
Integer 									ii_especie, ii_grupo, ii_subgrupo, ii_TipoOrd, ii_linea
Date										ldt_fechaIni, ldt_fechater
Transaction								itra_odbc
end variables

forward prototypes
public subroutine habilitadetalle (string as_columna)
protected function integer wf_modifica ()
public function boolean duplicado (string as_columna, string as_valor)
public subroutine habilitaencab (boolean habilita)
public function boolean existeprograma (string as_columna, string as_valor)
public subroutine habilitaingreso (string as_columna)
public subroutine buscaprograma ()
public function boolean noexistecliente (integer cliente)
public function boolean existeprocesos (date adt_fechaproc)
protected function boolean wf_actualiza_db (boolean borrando)
public function boolean existe_secuencia (integer ai_secuencia, long al_numero)
public function boolean conectacontrolodbc ()
public function boolean activaodbc (integer ai_linea)
public function boolean wf_genera_archivo_unitech (integer ai_cliente, long al_planta, integer ai_tipo, long al_orden)
end prototypes

public subroutine habilitadetalle (string as_columna);Boolean	lb_Estado = True



end subroutine

protected function integer wf_modifica ();IF dw_1.AcceptText() = -1 THEN RETURN -1
IF dw_2.AcceptText() = -1 THEN RETURN -1
IF (dw_1.ModifiedCount() + dw_1.DeletedCount()) > 0 THEN RETURN 0
IF dw_2.ModifiedCount() > 0 THEN RETURN 0

RETURN 1
end function

public function boolean duplicado (string as_columna, string as_valor);Long		ll_Fila, ll_Productor
Integer	li_Variedad, li_Periodo, li_Cliente
String	ls_TipoFrio

li_Variedad		=	dw_1.Object.vari_codigo[il_Fila]
ll_Productor	=	dw_1.Object.prod_codigo[il_Fila]
li_Periodo		=	dw_1.Object.pefr_codigo[il_Fila]
ls_TipoFrio		=	dw_1.Object.frio_tipofr[il_Fila]
li_Cliente		=	Integer(istr_mant.argumento[13])


CHOOSE CASE as_Columna
	CASE "vari_codigo"
		li_Variedad		=	Integer(as_valor)
		
	CASE "prod_codigo"
		ll_Productor	=	Long(as_valor)
		
	CASE "pefr_codigo"
		li_Periodo		=	Integer(as_valor)
		
	CASE "frio_tipofr"
		ls_TipoFrio		=	as_valor
		
END CHOOSE

IF Isnull(li_Variedad) OR Isnull(ll_Productor) OR Isnull(li_Periodo) OR Isnull(ls_TipoFrio) THEN
	RETURN False
ELSE
	ll_Fila	=	dw_1.Find("vari_codigo = "+String(li_Variedad)+" and " + &
								 "clie_codigo = "+String(li_Cliente)+" and " + &
								 "prod_codigo = "+String(ll_Productor)+" and " + &
								 "pefr_codigo = "+String(li_Periodo)+" and " + &
								 "frio_tipofr = '"+ls_TipoFrio+"'", 1, dw_1.RowCount())
//prueba de ingreso LMP	
//	IF ll_Fila > 0 AND ll_Fila <> il_Fila THEN
//		MessageBox("Atención","Detalle de Proceso ya fue especificado")
//		RETURN True
//	ELSE
		RETURN False
//	END IF
END IF
end function

public subroutine habilitaencab (boolean habilita);If Habilita Then
	dw_2.Object.espe_codigo.Protect					=	0
	dw_2.Object.ppre_numero.Protect					=	0
	dw_2.Object.ppre_fecpro.Protect					=	0
	dw_2.Object.busca_programa.Visible      	  	=  1
	dw_2.Object.clie_codigo.Protect					=	0
	
	dw_2.Object.espe_codigo.Color	=	0
	dw_2.Object.ppre_numero.Color	=	0
	dw_2.Object.ppre_fecpro.Color		=	0
	dw_2.Object.clie_codigo.Color		=	0
	
	dw_2.Object.espe_codigo.BackGround.Color	=	RGB(255,255,255)
	dw_2.Object.ppre_numero.BackGround.Color	=	RGB(255,255,255)
	dw_2.Object.ppre_fecpro.BackGround.Color		=	RGB(255,255,255)
	dw_2.Object.clie_codigo.BackGround.Color		=	RGB(255,255,255)
Else
	dw_2.Object.espe_codigo.Protect					=	1
	dw_2.Object.ppre_numero.Protect					=	1
	dw_2.Object.ppre_fecpro.Protect					=	1
	dw_2.Object.busca_programa.Visible        		=  0
	dw_2.Object.clie_codigo.Protect					=	1
	
	dw_2.Object.espe_codigo.Color	=	RGB(255,255,255)
	dw_2.Object.ppre_numero.Color	=	RGB(255,255,255)
	dw_2.Object.ppre_fecpro.Color		=	RGB(255,255,255)
	dw_2.Object.clie_codigo.Color		=	RGB(255,255,255)
	
	dw_2.Object.espe_codigo.BackGround.Color	=	553648127
	dw_2.Object.ppre_numero.BackGround.Color	=	553648127
	dw_2.Object.ppre_fecpro.BackGround.Color		=	553648127
	dw_2.Object.clie_codigo.BackGround.Color		=	553648127
End If
end subroutine

public function boolean existeprograma (string as_columna, string as_valor);Integer	li_Planta, li_Especie, li_Cliente
Long		ll_Numero
String	ls_Nombre
Boolean	lb_Retorno = True
Date		ldt_fecha

li_Planta		=	dw_2.Object.plde_codigo[1]
li_Especie	=	dw_2.Object.espe_codigo[1]
ll_Numero	=	dw_2.Object.ppre_numero[1]
li_Cliente		=	dw_2.Object.clie_codigo[1]

CHOOSE CASE as_columna
	CASE "plde_codigo"
		li_Planta	=	Integer(as_Valor)
		
	CASE "espe_codigo"
		li_Especie	=	Integer(as_Valor)
		
	CASE "ppre_numero"
		ll_Numero	=	Long(as_Valor)
		
END CHOOSE

IF Isnull(li_Planta) OR Isnull(li_Especie) OR IsNull(ll_Numero) THEN
	lb_Retorno	=	True
ELSE
	SELECT	ppre_nombre, ppre_feccre
		INTO	:ls_Nombre, :ldt_fecha
		FROM	dbo.spro_programaprocenca
		WHERE	plde_codigo	=	:li_Planta
		AND	espe_codigo	=	:li_Especie
		AND	ppre_numero	=	:ll_Numero
		AND   clie_codigo =  :li_Cliente;
		
	IF sqlca.SQLCode = -1 THEN
		F_ErrorBaseDatos(sqlca, "Lectura de Tabla de Programa de Procesos")
		
		lb_Retorno	=	False
	ELSEIF SQLCA.SQLCode = 100 THEN
		MessageBox("Atención","Programa no ha sido registrado")
		lb_Retorno	=	False
	ELSE
		dw_2.Object.ppre_nombre[1]	=	ls_Nombre
	
		PostEvent("ue_recuperadatos")
	END IF
END IF

RETURN lb_Retorno
end function

public subroutine habilitaingreso (string as_columna);Boolean	lb_Estado = True
Date		ldt_Fecha

IF as_Columna <> "ppre_numero" AND &
	(dw_2.Object.ppre_numero[1] = 0 OR IsNull(dw_2.Object.ppre_numero[1])) THEN
	lb_Estado = False
END IF

IF as_Columna <> "espe_codigo" AND &
	(dw_2.Object.espe_codigo[1] = 0 OR IsNull(dw_2.Object.espe_codigo[1])) THEN
	lb_Estado = False
END IF

IF as_Columna <> "ppre_fecpro" AND &
	(dw_2.Object.ppre_fecpro[1] = ldt_Fecha OR IsNull(dw_2.Object.ppre_fecpro[1])) THEN
	lb_Estado = False
END IF

dw_1.Enabled			=	lb_Estado
pb_ins_det.Enabled	=	lb_Estado
end subroutine

public subroutine buscaprograma ();Str_Busqueda	lstr_busq
Date				ldt_Fecha
Integer			li_Cliente

li_Cliente		=	dw_2.Object.clie_codigo[1]

lstr_busq.argum[1] =	String(dw_2.Object.plde_codigo[1])

IF ISnull(dw_2.Object.espe_codigo[1]) THEN
	lstr_busq.argum[2] = ""
ELSE
	lstr_busq.argum[2] = String(dw_2.Object.espe_codigo[1]) 
END IF

lstr_busq.argum[3] = String(li_Cliente) 
	
OpenWithParm(w_busc_spro_programaprocenca, lstr_busq)

lstr_busq	=	Message.PowerObjectParm

IF UpperBound(lstr_busq.argum) >= 5 THEN
	
	IF lstr_busq.argum[2] <> "" THEN
		istr_Mant.Argumento[2]	=	lstr_Busq.Argum[2]
		istr_Mant.Argumento[3]	=	lstr_Busq.Argum[3]
	
		dw_2.Object.espe_codigo[1]	=	Integer(lstr_Busq.Argum[2])
		dw_2.Object.ppre_numero[1]	=	Long(lstr_Busq.Argum[3])
		dw_2.Object.ppre_nombre[1]	=	lstr_Busq.Argum[5]
		dw_2.Object.ppre_fecpro[1] =	Date(lstr_Busq.Argum[4])
		
		istr_Mant.Argumento[4]	=	lstr_Busq.Argum[4]
		PostEvent("ue_recuperadatos")
	END IF
END IF
HabilitaIngreso("programa")
end subroutine

public function boolean noexistecliente (integer cliente);String ls_nombre

SELECT 	clie_nombre
INTO		:ls_nombre
FROM 		dbo.clientesprod
WHERE		clie_codigo =:cliente;

IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de Tabla ClientesProd")
	Return True
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención","Código No ha sido Generado. Ingrese Otro.")
	Return True
END IF

Return False
end function

public function boolean existeprocesos (date adt_fechaproc);Integer	li_Planta, li_Especie, li_Cantidad, li_Cliente
Long		ll_Numero
Boolean	lb_Retorno = True

li_Planta	=	dw_2.Object.plde_codigo[1]
li_Especie	=	dw_2.Object.espe_codigo[1]
ll_Numero	=	dw_2.Object.ppre_numero[1]
li_Cliente	=	dw_2.Object.clie_codigo[1]

SELECT	Count(sp.pprd_secuen)
	INTO	:li_Cantidad
	FROM	dbo.spro_progordenproceso as sp, 
			dbo.spro_programaprocenca as se
	WHERE	sp.plde_codigo	=	:li_Planta
	AND	sp.espe_codigo	=	:li_Especie
	AND	sp.ppre_numero	=	:ll_Numero
	AND   se.clie_codigo =  :li_Cliente	
	AND	sp.popr_fecpro	=	:adt_FechaProc
	AND   sp.plde_codigo	=	se.plde_codigo
	AND	sp.espe_codigo	=	se.espe_codigo
	AND	sp.ppre_numero	=  se.ppre_numero
	AND	se.clie_codigo	=	:li_Cliente;
	
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de Tabla de Programa de Ordenes Procesos")
	
	lb_Retorno	=	False
ELSEIF SQLCA.SQLCode = 100 THEN
	lb_Retorno	=	False
ELSE
	IF isnull(li_Cantidad) or li_cantidad=0 THEN lb_Retorno	=	False 	
END IF

RETURN lb_Retorno
end function

protected function boolean wf_actualiza_db (boolean borrando);Boolean	lb_AutoCommit, lb_Retorno

//IF Not dw_2.uf_check_required(0) THEN RETURN False

IF Not dw_1.uf_validate(0) THEN RETURN False

lb_AutoCommit		=	sqlca.AutoCommit
sqlca.AutoCommit	=	False

IF Borrando THEN
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
			IF dw_3.Update(True, False) = 1 THEN
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
			
			RollBack;
		END IF
	ELSE
		F_ErrorBaseDatos(sqlca, This.Title)
	END IF
END IF

sqlca.AutoCommit	=	lb_AutoCommit

RETURN lb_Retorno
end function

public function boolean existe_secuencia (integer ai_secuencia, long al_numero);Integer	li_Planta, li_Especie, li_Cantidad, li_Cliente, li_secuencia
Long		ll_Numero, ll_cont
Boolean	lb_Retorno = True

li_Planta	=	dw_2.Object.plde_codigo[1]
ll_Numero	=	dw_2.Object.ppre_numero[1]
li_Cliente	=	dw_2.Object.clie_codigo[1]

 SELECT count()
 INTO :ll_cont
  FROM dba.spro_programasalidadeta 
   WHERE plde_codigo = :li_planta  AND  
         orpr_numero = :al_numero  AND  
         clie_codigo = :li_cliente AND
			prsd_secuen = :ai_secuencia;

IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de Tabla de Programa de Ordenes Procesos")
	
	lb_Retorno	=	False
ELSEIF ll_cont = 0 THEN
	MessageBox("Error", "No existe Secuencia Ingresada.", StopSign!)
	lb_Retorno	=	False

END IF

RETURN lb_Retorno
end function

public function boolean conectacontrolodbc ();String	ls_Usuario, ls_Password, ls_nomodb, ls_nomser, ls_nombas, ls_nodbms, ls_planta, ls_DbParm, ls_IP

IF ib_conexionodbc THEN
	DISCONNECT USING itra_odbc;
END IF

ls_Usuario				=	sqlca.UserId
ls_Password				=	sqlca.DbPass

SELECT cone_nomodb,cone_nomser,cone_nombas,
		cone_nodbms,cone_nomusu,cone_passwo, cone_ipserv
 INTO :ls_nomodb,:ls_nomser,:ls_nombas,
		:ls_nodbms,:ls_Usuario,:ls_Password, :ls_IP
 FROM dbo.prodconectividad   
WHERE cone_codigo = :gstr_paramplanta.controlodbc;

IF IsNull(ls_nomodb) OR ls_nomodb = "" THEN
	MessageBox("Error ODBC", "El ODBC marcado para el BD ControlODBC no se encuentra en tabla de conectividad,"+&
									 "Por favor, comuniquese con el administrador del sistema", Exclamation!)
END IF

itra_odbc.DataBase		=	ls_nombas
itra_odbc.Dbms				= 	ls_nodbms
	
If ls_nodbms = 'ODBC' Then
	itra_odbc.ServerName	=	ls_nomser
	itra_odbc.DbParm			=	"Connectstring='DSN=" + ls_nomodb + ";UID=" + ls_Usuario + &
								";PWD=" + ls_Password + "'// ;PBUseProcOwner = " + '"Yes"'
								
ElseIf Mid(ls_nodbms,1,3) = 'SNC' or Mid(ls_nodbms,1,9) = 'TRACE SNC' Then
	itra_odbc.ServerName	=	ls_IP
	itra_odbc.LogId   			= ls_Usuario
	itra_odbc.LogPass			= ls_Password
	itra_odbc.Autocommit = True
	
	itra_odbc.DBParm = "Provider='SQLNCLI10',Database='"+ls_nombas+"',TrimSpaces=1"
End If
								
CONNECT USING itra_odbc;

IF itra_odbc.SQLCode = 0 THEN
	ib_conexionodbc	=	True
ELSE
	ib_conexionodbc	=	False
END IF

RETURN ib_conexionodbc

end function

public function boolean activaodbc (integer ai_linea);Integer	li_cliente, li_conexion

li_cliente	=	dw_2.Object.clie_codigo[1]

SELECT cone_codigo INTO :li_conexion
  FROM dbo.conectividad
 WHERE cone_codigo = :li_cliente
   AND cone_lineas = :ai_linea
 USING itra_odbc;
 
IF IsNull(li_conexion) THEN
	MessageBox("Error", "Se ha producido un error al activar ODBC para cajasprod, "+&
							  "ya que no existe ODBC para este cliente", StopSign!)
							  
	Rollback;
	Return False
ELSE
	UPDATE dbo.conectividad
		SET cone_estado = 1
	 WHERE cone_codigo = :li_cliente
		AND cone_lineas = :ai_linea
	 USING itra_odbc;
	 
	 Commit;
	 Return True
END IF
end function

public function boolean wf_genera_archivo_unitech (integer ai_cliente, long al_planta, integer ai_tipo, long al_orden);Boolean	lb_Retorno = True
Long		ll_Fila
String 	ls_Archivo, ls_Ruta

SetPointer(HourGlass!)

dw_Unitech.SetTransObject(Sqlca)

ll_Fila = dw_Unitech.Retrieve(ai_Cliente, al_Planta, ai_Tipo, al_Orden)

If ll_Fila = -1 Then 
	lb_Retorno = False
ElseIf ll_Fila = 0 Then
	lb_Retorno = False
Else	
	RegistryGet( "HKEY_CURRENT_USER\Software\Microsoft\Windows\CurrentVersion\Explorer\Shell Folders", "Personal", RegString!, ls_Ruta)
	ls_Archivo	=	ls_Ruta + "\" + String(al_Orden, '00000000') + '.ord'
	
	If dw_Unitech.SaveAs(ls_Archivo, CSV!, False) = -1 Then
		MessageBox('Error', 'No se pùdo generar archivo ('+ ls_Archivo +') con información solicitda.' , StopSign!, OK! )
		lb_Retorno = False
	Else
		MessageBox('Atencion', 'Archivo ('+ ls_Archivo +') generado satisfactoriamente.' , Information!, OK! )
	End If
End IF

SetPointer(Arrow!)

Return lb_Retorno
end function

on w_maed_activa_programacion.create
int iCurrent
call super::create
this.dw_3=create dw_3
this.dw_unitech=create dw_unitech
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_3
this.Control[iCurrent+2]=this.dw_unitech
end on

on w_maed_activa_programacion.destroy
call super::destroy
destroy(this.dw_3)
destroy(this.dw_unitech)
end on

event open;/* 
Argumentos
istr_Mant.Argumento[1]	=	Código Planta
istr_Mant.Argumento[2]	=	Especie
istr_Mant.Argumento[3]	=	Número Programa
istr_Mant.Argumento[4]	=	Fecha Proceso
istr_Mant.Argumento[5]	=	Variedad
istr_Mant.Argumento[6]	=	Productor
istr_Mant.Argumento[7]	=	Periodo Frio
istr_Mant.Argumento[8]	=	Tratamiento Frio
istr_Mant.Argumento[9]	=	Numero de Orden
istr_Mant.Argumento[13]	=	Cliente
*/

x				= 0
y				= 0
This.Height	= 2520
im_menu		= m_principal
ii_TipoOrd 	= -1//Integer(Message.StringParm)

This.ParentWindow().ToolBarVisible	= True
im_menu.Item[1].Item[6].Enabled		= True
im_menu.Item[7].Visible					= True

iuo_especie									=	Create uo_especie
iuo_variedad								=	Create uo_variedades
iuo_grupo									=	Create uo_grupoespecie
iuo_subgrupo								=	Create uo_subgrupoespecie
iuo_ordenproceso							=	Create uo_spro_ordenproceso
iuo_productores							=	Create uo_productores
iuo_tratamientofrio						=	Create uo_tratamientofrio
iuo_periodofrio							=	Create uo_periodofrio
iuo_lineapacking							=	Create uo_lineapacking
iuo_lotesfrutagranel						=	Create uo_lotesfrutagranel
iuo_etiquetas								= 	Create uo_etiquetas
itra_odbc									=	Create Transaction

istr_Mant.Argumento[1]					=	String(gstr_ParamPlanta.CodigoPlanta)
istr_Mant.Argumento[2]					=	String(gi_codespecie)
istr_Mant.Argumento[3]  				=  String(Today(),'dd/mm/yyyy')
istr_Mant.Argumento[13] 				=  String(gi_codexport)

dw_3.SetTransObject(Sqlca)
dw_2.SetTransObject(Sqlca)
dw_1.SetTransObject(Sqlca)

dw_2.GetChild("plde_codigo", idwc_planta)
idwc_planta.SetTransObject(sqlca)

IF idwc_planta.Retrieve(gi_codexport) = 0 THEN
	idwc_planta.InsertRow(0)
END IF

dw_2.GetChild("espe_codigo", idwc_especie)
idwc_especie.SetTransObject(sqlca)
IF idwc_especie.Retrieve() = 0 THEN
	MessageBox("Atención","Falta Registrar Especies")
	idwc_especie.InsertRow(0)
ELSE
	idwc_especie.SetSort("espe_nombre A")
	idwc_especie.Sort()
END IF

dw_1.GetChild("vari_codigo",idwc_variedad)
idwc_variedad.SetTransObject(Sqlca)
IF idwc_variedad.Retrieve(gstr_paramplanta.codigoespecie) = 0 THEN
	MessageBox("Atención","Falta Registrar Variedades")
	idwc_variedad.InsertRow(0)
ELSE
	idwc_variedad.SetSort("vari_nombre A")
	idwc_variedad.Sort()
END IF

dw_1.Getchild("prod_codigo", idwc_productor)
idwc_productor.SetTransObject(sqlca)
IF idwc_productor.Retrieve() = 0 THEN
	MessageBox("Atención","Falta Registrar Productores")
	idwc_productor.InsertRow(0)
ELSE
	idwc_productor.SetSort("prod_nombre A")
	idwc_productor.Sort()
END IF

dw_1.Getchild("pefr_codigo", idwc_periodofrio)
idwc_periodofrio.SetTransObject(sqlca)
IF idwc_periodofrio.Retrieve() = 0 THEN
	MessageBox("Atención","Falta Registrar Período Frío")
	idwc_periodofrio.InsertRow(0)
ELSE
	idwc_periodofrio.SetSort("pefr_nombre A")
	idwc_periodofrio.Sort()
END IF

dw_1.Getchild("frio_tipofr", idwc_tratamiento)
idwc_tratamiento.SetTransObject(sqlca)
IF idwc_tratamiento.Retrieve() = 0 THEN
	MessageBox("Atención","Falta Registrar Tratamiento Frío")
	idwc_tratamiento.InsertRow(0)
ELSE
	idwc_tratamiento.SetSort("frio_nombre A")
	idwc_tratamiento.Sort()
END IF

dw_1.Getchild("line_codigo", idwc_lineapacking)
idwc_lineapacking.SetTransObject(sqlca)
IF idwc_lineapacking.Retrieve(gstr_ParamPlanta.CodigoPlanta) = 0 THEN
	MessageBox("Atención","Falta Registrar Linea Packing")
	idwc_lineapacking.InsertRow(0)
ELSE
	idwc_lineapacking.SetSort("line_nombre A")
	idwc_lineapacking.Sort()
END IF

dw_2.GetChild("prsa_lineaa", idwc_linea)
idwc_linea.SetTransObject(sqlca)
IF idwc_linea.Retrieve(gstr_ParamPlanta.CodigoPlanta) = 0 THEN
	MessageBox("Atención","Falta Registrar Linea Packing")
	idwc_linea.InsertRow(0)
ELSE
	idwc_linea.SetSort("#1 A")
	idwc_linea.Sort()
	ii_linea							=	idwc_linea.GetItemNumber(1, "line_codigo")
	dw_2.Object.prsa_lineaa[1] = 	ii_linea
END IF


//Cliente
dw_2.GetChild("clie_codigo", idwc_cliente)
idwc_cliente.SetTransObject(sqlca)
idwc_cliente.Retrieve()
idwc_cliente.InsertRow(0)

istr_Mant.solo_consulta =	False

Paramtemporada(gstr_paramtempo)
ldt_fechaIni = gstr_paramtempo.fechainicio
ldt_fechater = gstr_paramtempo.fechatermino

pb_nuevo.PostEvent(Clicked!)
end event

event ue_recuperadatos;Long		ll_fila_d, ll_fila_e, respuesta
Date		ldt_FechaProc

ldt_FechaProc					=	Date(Mid(istr_Mant.Argumento[4], 1, 10))
istr_Mant.Argumento[13] 	= 	String(dw_2.object.clie_codigo[1])

DO
	ll_fila_e	=	dw_2.Retrieve(Integer(istr_Mant.Argumento[1]), Integer(istr_Mant.Argumento[2]), &
				  Long(istr_Mant.Argumento[3]), Integer(istr_Mant.Argumento[13])) 
										  
	IF ll_fila_e = -1 THEN
		respuesta = MessageBox("Error en Base de Datos", "No es posible conectar la Base de Datos.", &
						Information!, RetryCancel!)		
	ELSE
		DO
			ll_fila_e	=	dw_1.Retrieve(Integer(istr_Mant.Argumento[1]), Integer(istr_Mant.Argumento[2]), &
												  ldt_FechaProc, Long(istr_Mant.Argumento[3]),&
												  Integer(istr_Mant.Argumento[13]), ii_linea) 
											  
			IF ll_fila_e = -1 THEN
				respuesta = MessageBox("Error en Base de Datos", "No es posible conectar la Base de Datos.",  Information!, RetryCancel!)		
			ELSE
				pb_eliminar.Enabled			=	True
				pb_grabar.Enabled			=	True
				pb_imprimir.Enabled			=	True
				pb_ins_det.Enabled			=	True
				pb_eli_det.Enabled			=	True
				il_Fila							=	1
				
				dw_2.Object.ppre_fecpro[1]	=	ldt_FechaProc
				dw_2.Object.prsa_lineaa[1]	=	ii_linea
				
				HabilitaEncab(False)
				dw_1.SetRow(il_Fila)
				dw_1.SetFocus()
			END IF
			
		LOOP WHILE respuesta = 1
		IF respuesta = 2 THEN Close(This)
		
	END IF
LOOP WHILE respuesta = 1

IF respuesta = 2 THEN Close(This)
end event

event ue_modifica_detalle;Long 		ll_fila
Integer	li_estant
String 	ls_estado

DO
	ll_fila	=	dw_1.GetSelectedRow(ll_fila)
	IF ll_fila > 0 THEN
		
		IF dw_1.Object.prsa_estado[ll_fila] = 0 THEN
			
			li_estant = dw_1.Object.prsa_estado[ll_fila]
			dw_1.Object.prsa_estado[ll_fila] = 1
			
		ELSEIF dw_1.Object.prsa_estado[ll_fila] = 1 THEN
			
			li_estant = dw_1.Object.prsa_estado[ll_fila]
			dw_1.Object.prsa_estado[ll_fila] = 2
			
		ELSEIF dw_1.Object.prsa_estado[ll_fila] = 2 THEN
			
			li_estant = dw_1.Object.prsa_estado[ll_fila]
			dw_1.Object.prsa_estado[ll_fila] = 1
			
		END IF
		
		CHOOSE CASE li_estant
			CASE 1
				ls_estado	=	"Activo"
				
			CASE 2
				ls_estado	=	"Terminado"
				
			CASE 0
				ls_estado	=	"En Programación"
				
		END CHOOSE
		
		CHOOSE CASE dw_1.Object.prsa_estado[ll_fila]
			CASE 1
				ls_estado	=	ls_estado + " a Activo"
				
			CASE 2
				ls_estado	=	ls_estado + " a Terminado"
				

		END CHOOSE
		
		IF MessageBox("Confirmación", "Desea cambiar el estado de la Programción de la orden " + &
												  String(dw_1.Object.orpr_numero[ll_fila]) + " desde " + &
												  ls_estado, Question!, YesNo!, 2) = 2 THEN
			dw_1.Object.prsa_estado[ll_fila]	=	li_estant
		END IF
		
	END IF
	
LOOP WHILE ll_fila > 0

end event

event ue_borrar;IF dw_1.RowCount() < 1 THEN RETURN

SetPointer(HourGlass!)

ib_borrar = True
w_main.SetMicroHelp("Validando la eliminación...")

Message.DoubleParm = 0

This.TriggerEvent ("ue_validaborrar")

IF Message.DoubleParm = -1 THEN RETURN

IF dw_1.RowCount() > 0 THEN dw_1.RowsMove(1,dw_1.RowCount(),Primary!,dw_1,1,Delete!)

ib_borrar = False
w_main.SetMicroHelp("Borrando Registro...")
		
IF wf_actualiza_db(True) THEN
	w_main.SetMicroHelp("Registro Borrado...")
	This.TriggerEvent("ue_nuevo")
	SetPointer(Arrow!)
ELSE
	w_main.SetMicroHelp("Registro no Borrado...")
END IF
end event

event ue_seleccion;call super::ue_seleccion;str_Busqueda	lstr_Busq

lstr_Busq.Argum[1]	=	istr_Mant.Argumento[1]
lstr_Busq.Argum[2]	=	'0'
lstr_Busq.Argum[3]	=	String(ii_TipoOrd)
lstr_Busq.Argum[4]	=	istr_Mant.Argumento[13]

OpenWithParm(w_busc_spro_ordenproceso, lstr_busq)

lstr_busq	= Message.PowerObjectParm

IF lstr_busq.argum[3] <> "" THEN
	istr_mant.argumento[2] 	= 	lstr_busq.argum[4]
	istr_mant.argumento[3] 	= 	lstr_Busq.Argum[11]
	istr_mant.argumento[4] 	= 	lstr_busq.argum[5]
	ii_TipoOrd					=	Integer(lstr_busq.argum[10])
	
	dw_2.Object.plde_codigo[1]	=	Integer(lstr_Busq.Argum[3])
	dw_2.Object.espe_codigo[1]	=	Integer(lstr_Busq.Argum[4])
	dw_2.Object.ppre_numero[1]	=	Long(lstr_Busq.Argum[11])
	dw_2.Object.ppre_fecpro[1]	=	Date(Mid(lstr_busq.argum[5],1,10))
	
	IF ExistePrograma("ppre_numero",lstr_Busq.Argum[11]) THEN
	
		TriggerEvent("ue_recuperadatos")
	END IF
	
	dw_1.SetFocus()
	
ELSE
	pb_buscar.SetFocus()
END IF

end event

event ue_antesguardar;Long			ll_Fila, ll_Numero, ll_NroProc, ll_NroProcAnt, ll_find, ll_linact[4], ll_linant
Integer		li_Planta, li_Especie, li_Secuencia, li_Contador
Date			ldt_FechaProc
String			ls_Mensaje, ls_Columna[]

li_Planta				=	Integer(istr_Mant.Argumento[1])
li_Especie			=	Integer(istr_Mant.Argumento[2])
ll_Numero			=	Long(istr_Mant.Argumento[3])	
ldt_FechaProc		=	dw_2.Object.ppre_fecpro[1]
ll_linact[1]			=	0
ll_linact[2]			=	0
ll_linact[3]			=	0
ll_linact[4]			=	0

Message.DoubleParm 	= 	0

IF dw_1.RowCount() < 1 THEN Return

FOR ll_Fila = 1 TO dw_1.RowCount()
	ll_NroProc						=	dw_1.object.orpr_numero[ll_Fila]
	ll_linant						=	dw_1.object.prsa_lineaa[ll_Fila]
	IF dw_1.Object.prsa_estado[ll_fila] = 1 AND ll_NroProcAnt <> ll_NroProc THEN
		li_Contador 				++
		ls_Mensaje 					+=	"~nProceso " + String(dw_1.Object.orpr_numero[ll_fila])
		ll_NroProcAnt				=	dw_1.object.orpr_numero[ll_Fila]
		ll_linact[ll_linant]		=	ll_linact[ll_linant] + 1
	END IF
NEXT

FOR ll_linant = 1 TO 4
	IF ll_linact[ll_linant] > 1 THEN
		IF li_Contador > 1 THEN
			MessageBox("Error de Consistencia", "Hay mas de una Programación Activa :" + ls_Mensaje + ".", StopSign!, Ok!)
			Message.DoubleParm 	=	 -1
			dw_1.SetRow(ll_fila)
			dw_1.SetFocus()
			RETURN
		END IF
	END IF
NEXT

FOR ll_linant = 1 TO dw_1.RowCount()
	IF dw_1.Object.prsa_estado[ll_linant] = 1 THEN
		dw_3.Retrieve(li_Planta, ldt_FechaProc, dw_1.object.prsa_lineaa[ll_linant] )
		dw_3.SetFilter("prsa_estado = 1 AND orpr_numero <> " + String(dw_1.Object.orpr_numero[ll_linant]))
		dw_3.Filter()
		
		li_Contador 	= 	0
		ls_Mensaje 	= 	''
		
		FOR ll_Fila = 1 TO dw_3.RowCount()
			IF dw_1.object.prsa_lineaa[ll_linant] = dw_3.object.prsa_lineaa[ll_Fila] THEN
				li_Contador ++
				ls_Mensaje 					+=	"~nProceso " 			+ String(dw_3.Object.orpr_numero[ll_fila], '000')
				ls_Mensaje 					+=	" Fecha " 				+ String(dw_3.Object.orpr_fecpro[ll_fila])
				ls_Mensaje 					+=	" Programa Proceso " + String(dw_3.Object.ppre_numero[ll_fila], '00')
				ls_Mensaje 					+=	" Linea " 				+ String(dw_3.Object.prsa_lineaa[ll_fila])
			END IF
		NEXT
		
		IF li_Contador > 0 THEN
			IF MessageBox("Error de Consistencia", "Hay mas de una Programación Activa :" + ls_Mensaje + ". ¿Desea Terminar estas Programaciones?.", Question!, YesNo!) = 1 THEN
				FOR ll_Fila = 1 TO dw_3.RowCount()
					dw_3.Object.prsa_estado[ll_Fila] = 3
				NEXT
			ELSE
				Message.DoubleParm 	=	 -1
				dw_1.SetFocus()
				RETURN
			END IF
		END IF
	END IF
NEXT

FOR ll_Fila = 1 TO dw_1.RowCount()
	IF dw_1.GetItemStatus(ll_Fila, 0, Primary!) = NewModified! THEN
		ll_NroProc								=	dw_1.Object.orpr_numero[ll_Fila]

		dw_1.Object.plde_codigo[ll_Fila]	=	li_Planta
		dw_1.Object.orpr_tipord[ll_Fila]	=	ii_TipoOrd
		dw_1.Object.clie_codigo[ll_fila] 	=  Integer(istr_mant.argumento[13])
	END IF
	
	IF dw_1.Object.prsa_estado[ll_fila] = 1 THEN
		IF ConectaControlODBC() THEN
			IF NOT ActivaODBC(dw_1.object.prsa_lineaa[ll_fila]) THEN
				dw_1.SetFocus()
				
				DISCONNECT USING itra_odbc;
				Message.DoubleParm 	=	 -1
				
				RETURN
			END IF
		ELSE
			MessageBox("Error", "No es posible realizar la conexión con BD control de ODBC", StopSign!)
			Message.DoubleParm 	=	 -1
			dw_1.SetFocus()
			RETURN
		END IF
	END IF
NEXT

IF ib_conexionodbc THEN
	DISCONNECT USING itra_odbc;
END IF
end event

event resize;call super::resize;Integer	maximo, li_posic_x, li_posic_y, li_visible = 0

IF dw_2.width > il_AnchoDw_1 THEN
	maximo		=	dw_2.width
ELSE
	dw_1.width	=	This.WorkSpaceWidth() - 400
	maximo		=	dw_1.width
END IF

dw_2.x					= 37 + Round((maximo - dw_2.width) / 2, 0)
dw_2.y					= 37

dw_1.x					= 37 + Round((maximo - dw_1.width) / 2, 0)
dw_1.y					= 64 + dw_2.Height
dw_1.height			= This.WorkSpaceHeight() - dw_1.y - 41


end event

event ue_nuevo;call super::ue_nuevo;Long		ll_modif

ib_ok	= True

istr_mant.Solo_Consulta	=	istr_mant.UsuarioSoloConsulta

IF Not istr_mant.Solo_Consulta THEN
	CHOOSE CASE wf_modifica()
		CASE -1
			ib_ok = False
		CASE 0
			ll_modif	=	dw_1.GetNextModified(0, Primary!)
			
			IF dw_1.RowCount() > 0 and ll_modif > 0 THEN
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
pb_grabar.Enabled		=	False
pb_eliminar.Enabled		=	False
pb_imprimir.Enabled		=	False
dw_2.Enabled				=	True
lb_Actualiza 				=  False

dw_2.SetRedraw(False)
dw_2.Reset()
dw_2.InsertRow(0)
dw_2.SetRedraw(True)

dw_2.SetFocus()

dw_2.Object.plde_codigo[1]	=			(gstr_ParamPlanta.CodigoPlanta)

istr_Mant.Argumento[1]			=	String	(gstr_ParamPlanta.CodigoPlanta)
istr_Mant.Argumento[2]			=	String	(gi_Codespecie)
istr_Mant.Argumento[3]  		=  String	(Today(),'dd/mm/yyyy')

HabiliTaEncab(True)
end event

type dw_1 from w_mant_encab_deta`dw_1 within w_maed_activa_programacion
integer x = 119
integer y = 784
integer width = 2002
integer height = 1124
integer taborder = 10
string title = "Programas Salidas"
string dataobject = "dw_mant_muestra_programa_salidas"
end type

event dw_1::doubleclicked;//
end event

event dw_1::rowfocuschanged;//
end event

event dw_1::clicked;IF Row > 0 THEN
	il_fila = Row
	THIS.SelectRow(row, NOT THIS.IsSelected(row))
END IF

RETURN 0
end event

event dw_1::getfocus;//
end event

event dw_1::itemerror;RETURN 1
end event

event dw_1::ue_seteafila;//
end event

event dw_1::buttonclicked;call super::buttonclicked;Integer			li_Nula
String				ls_Columna, ls_Nula
Date				ldt_FechaProc, ldt_FechaSistema, ldt_fecha
str_busqueda	lstr_busq

ls_Columna = dwo.Name

SetNull(ls_Nula)

CHOOSE CASE ls_Columna
	CASE "b_programa"	
		lstr_busq.argum[1]	=	istr_Mant.Argumento[1]
		lstr_busq.argum[2]   =	istr_mant.Argumento[13]
		lstr_busq.argum[3]	=	String(This.Object.orpr_tipord[row])
		lstr_busq.argum[4]	=	String(This.Object.orpr_numero[row])
		
		OpenWithParm(w_busc_secuenciaprograma, lstr_busq)
		
		lstr_busq	= Message.PowerObjectParm
		
		IF UpperBound(lstr_busq.argum) > 3 THEN
			IF lstr_busq.argum[3] <> "" THEN
				This.Object.prsa_lineaa[row]	=	Integer(lstr_busq.argum[1])
			END IF
		ELSE
			Return -1
		END IF
		Return 1

	Case 'b_archivo'
		If Not wf_Genera_Archivo_Unitech(dw_2.Object.clie_codigo[1], dw_2.Object.plde_codigo[1], &
				This.Object.orpr_tipord[Row], This.Object.orpr_numero[Row]) Then
			MessageBox('Error', 'No se pudo generar archivo para UNITECH', StopSign!, Ok!)
			Return -1
		End If
	
END CHOOSE
end event

event dw_1::itemchanged;call super::itemchanged;Integer li_nula
String ls_columna


ls_Columna = dwo.Name
SetNull(li_nula)

CHOOSE CASE ls_Columna
	CASE "prsa_lineaa"
		IF not existe_secuencia(Integer(data),dw_1.Object.orpr_numero[row]) THEN
			This.SetItem(row,"prsa_lineaa",li_Nula)
			RETURN 1
		END IF
		
	
END CHOOSE
end event

type dw_2 from w_mant_encab_deta`dw_2 within w_maed_activa_programacion
integer x = 119
integer y = 44
integer width = 2002
integer height = 664
integer taborder = 90
string dataobject = "dw_sel_prograproceso_enca_programacion"
end type

event dw_2::itemchanged;call super::itemchanged;Integer	li_Nula
String	ls_Columna, ls_Nula
Date		ldt_FechaProc, ldt_FechaSistema, ldt_fecha

ls_Columna = dwo.Name

SetNull(li_Nula)
SetNull(ls_Nula)

CHOOSE CASE ls_Columna
	CASE "clie_codigo"
		IF NoExisteCliente(Integer(data)) THEN
			This.SetItem(row,"clie_codigo",Integer(ls_Nula))
			RETURN 1
		END IF
		istr_mant.Argumento[13] = data

	CASE "ppre_numero"
		IF NOT Existeprograma(ls_columna, data) THEN
			This.SetItem(row,ls_columna,li_Nula)
			RETURN 1
		ELSE
			istr_mant.argumento[3] = Data
		END IF	

	CASE "espe_codigo"	
		istr_mant.argumento[2] = ""
		IF NOT iuo_Especie.Existe(Integer(Data),True,SqlCa) THEN  
			ii_especie	= integer(ls_Nula)
			istr_mant.argumento[2] = ""
			This.SetItem(row,"espe_codigo", li_Nula)
			This.SetFocus()
			RETURN 1
	   ELSE
			IF NOT Existeprograma(ls_columna, data) THEN
				This.SetItem(row,"espe_codigo", li_Nula)
				istr_mant.argumento[2] = ""
				ii_especie	= integer(ls_Nula)
				This.SetFocus()
				RETURN 1
			ELSE
				ii_especie = Integer(Data)
				istr_mant.argumento[2] = Data
			END IF	
		END IF	
	
	CASE "ppre_fecpro"
		ldt_FechaProc		=	Date(Mid(Data, 1, 10))
		SetNull(ldt_fecha)
		IF ldt_FechaProc > ldt_fechater THEN
			MessageBox("Atención","Fecha de Movimiento no puede ser superior a Fecha de término de temporada.")
			This.SetItem(row,"ppre_fecpro", ldt_fecha)
			RETURN 1
		ELSE	
			IF ldt_FechaProc < ldt_fechaini THEN
				MessageBox("Atención","Fecha de Movimiento no puede ser menor a Fecha de inicio de temporada.")
				This.SetItem(row,"ppre_fecpro", ldt_fecha)
				RETURN 1
			ELSE
				istr_Mant.Argumento[4]	=	String(Date(Mid(Data,1,10)))
				IF ExisteProcesos(ldt_FechaProc) THEN
					Parent.PostEvent("ue_recuperadatos")
				END IF	
			END IF	
		END IF
		
	CASE "prsa_lineaa"
		ii_linea	=	Integer(Data)
		Parent.PostEvent("ue_recuperadatos")
END CHOOSE

HabilitaIngreso(ls_Columna)
end event

event dw_2::buttonclicked;call super::buttonclicked;CHOOSE CASE dwo.Name
	CASE "busca_programa"
		BuscaPrograma()

END CHOOSE
end event

event dw_2::clicked;//
end event

event dw_2::doubleclicked;//
end event

type pb_nuevo from w_mant_encab_deta`pb_nuevo within w_maed_activa_programacion
integer x = 2181
integer y = 268
integer taborder = 40
end type

type pb_eliminar from w_mant_encab_deta`pb_eliminar within w_maed_activa_programacion
boolean visible = false
integer x = 2181
integer y = 448
integer taborder = 50
end type

type pb_grabar from w_mant_encab_deta`pb_grabar within w_maed_activa_programacion
integer x = 2181
integer y = 452
integer taborder = 60
end type

type pb_imprimir from w_mant_encab_deta`pb_imprimir within w_maed_activa_programacion
boolean visible = false
integer x = 2181
integer y = 640
integer taborder = 70
end type

type pb_salir from w_mant_encab_deta`pb_salir within w_maed_activa_programacion
integer x = 2181
integer y = 840
integer taborder = 80
end type

type pb_ins_det from w_mant_encab_deta`pb_ins_det within w_maed_activa_programacion
string tag = "Activa Programa"
boolean visible = false
integer x = 2171
integer y = 1320
integer taborder = 20
string picturename = "\Desarrollo 17\Imagenes\Botones\Aceptar.png"
string disabledname = "\Desarrollo 17\Imagenes\Botones\Aceptar-bn.png"
end type

event pb_ins_det::clicked;Parent.TriggerEvent("ue_modifica_detalle")
end event

type pb_eli_det from w_mant_encab_deta`pb_eli_det within w_maed_activa_programacion
string tag = "Desactiva Programa"
boolean visible = false
integer x = 2181
integer y = 1588
integer taborder = 100
string picturename = "\Desarrollo 17\Imagenes\Botones\Cancelar.png"
string disabledname = "\Desarrollo 17\Imagenes\Botones\Cancelar-bn.png"
end type

type pb_buscar from w_mant_encab_deta`pb_buscar within w_maed_activa_programacion
integer x = 2181
integer taborder = 30
end type

type dw_3 from datawindow within w_maed_activa_programacion
boolean visible = false
integer x = 119
integer y = 1308
integer width = 1879
integer height = 600
integer taborder = 80
boolean bringtotop = true
boolean enabled = false
boolean titlebar = true
string title = "none"
string dataobject = "dw_programa_salidas_activos"
boolean hscrollbar = true
boolean vscrollbar = true
boolean resizable = true
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type dw_unitech from uo_dw within w_maed_activa_programacion
boolean visible = false
integer x = 2185
integer y = 1124
integer width = 197
integer height = 176
integer taborder = 11
boolean bringtotop = true
string dataobject = "dw_gene_ordenunitech"
boolean vscrollbar = false
end type

