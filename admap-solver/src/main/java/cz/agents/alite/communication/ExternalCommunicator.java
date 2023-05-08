package cz.agents.alite.communication;

import cz.agents.alite.communication.content.Content;

/**
 * Communicator set to delegate all the work regarding communication channel and message passing to external listener
 */
public class ExternalCommunicator implements Communicator {
    private final MessageHandler senderHandler;

    private final int id;

    private int messageCount;

    public ExternalCommunicator(MessageHandler senderHandler, int id) {
        this.senderHandler = senderHandler;
        this.id = id;
        this.messageCount = 0;
    }

    @Override
    public Message createMessage(Content content) {
        return new Message(String.valueOf(id), content, messageCount++);
    }

    @Override
    public Message createReply(Message message, Content content) {
        return new Message(String.valueOf(id), content, message.getId());
    }

    @Override
    public void addMessageHandler(MessageHandler messageHandler) {
        // Not used for external communicator
    }

    @Override
    public void removeMessageHandler(MessageHandler messageHandler) {
        // Not used for external communicator
    }

    @Override
    public void receiveMessage(Message message) {
        // Not used for external communicator
    }
    
    @Override
    public void sendMessage(Message message) {
        senderHandler.notify(message);
    }

    @Override
    public String getAddress() {
        return null;
    }

    public static Message createMessageFromPicoMessage(String sender, Content content, long id) {
        return new Message(sender, content, id);
    }
}
